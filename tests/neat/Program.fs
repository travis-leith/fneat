open Expecto
open neat

module Expect =
    let exn_message f m =
        let failureMessage =
            try
                f()
                None
            with
            |exn -> Some exn.Message
        Expect.equal failureMessage (Some m) ""

let add_connection_by_int (network:Network) in_id out_id weight global_innovation =
    let net, inn = Network.add_connection network (NodeId in_id) (NodeId out_id) weight global_innovation
    net, inn

let add_node_by_in_out (network:Network) in_id out_id new_weight global_innovation =
    let conn_index =
        match network.genome |> Seq.tryFindIndex (fun x -> x.in_node.id.int = in_id && int x.out_node.id.int = out_id && x.enabled) with
        |Some i -> i
        |None -> failwith "Cannot add a node to a connection that does not exist"

    let new_conn_index = network.genome.Count
    let new_network, global_innov = Network.add_node network conn_index global_innovation
    new_network.genome[new_conn_index].weight <- new_weight
    new_network, global_innov

let diable_connection_by_in_out (network:Network) in_id out_id =
    let conn_index =
        match network.genome |> Seq.tryFindIndex (fun x -> x.in_node.id.int = in_id && x.out_node.id.int = out_id && x.enabled) with
        |Some i -> i
        |None -> failwith "Cannot disable a connection that does not exist"

    network.genome[conn_index].enabled <- false
    network

let network_creation = test "network creation" {
    let n_sensors = 3
    let n_outputs = 2
    let r = System.Random()
    let network = Network.init r n_sensors n_outputs

    Expect.equal network.output_nodes.Count n_outputs ""
    Expect.equal network.sensor_nodes.Count n_sensors ""
    Expect.equal network.hidden_nodes.Count 0 ""
    network.output_nodes |> Seq.iter (fun node ->
        Expect.equal node.inputs.Length n_sensors ""
    )
}

let cannot_add_connection_out_to_sensor = test "cannot_add_connection_out_to_sensor" {
    let r = System.Random()
    let network = Network.init r 2 2
    Expect.exn_message (fun () -> add_connection_by_int network 1 0 0. (InnovationNumber 0) |> ignore) "Tried to add a connection that outputs to a sensor node"
}

let cannot_add_connection_in_from_output = test "cannot_add_connection_in_from_output" {
    let r = System.Random()
    let network = Network.init r 2 2
    Expect.exn_message (fun () -> add_connection_by_int network 2 3 0. (InnovationNumber 0) |> ignore) "Tried to add a connection that inputs from an output node"
}

let cannot_add_connection_beyond_length = test "cannot_add_connection_beyond_length" {
    let r = System.Random()
    let network = Network.init r 2 2
    Expect.exn_message (fun () -> add_connection_by_int network 1 5 0. (InnovationNumber 0) |> ignore) "Tried to add a connection with an output (5) that skips an available id (4)"
}

let cannot_add_preexisting_connection = test "cannot_add_preexisting_connection" {
    let r = System.Random()
    let network = Network.init r 2 2
    Expect.exn_message (fun () -> add_connection_by_int network 1 2 0.5 (InnovationNumber 0) |> ignore) "Tried to connect 2 ndoes that are already connected"
}
let can_add_valid_node = test "can_add_valid_node" {
    let r = System.Random()
    let network = Network.init r 2 2
    let existing_conn_weight= network.genome[0].weight
    let network, global_innvation = Network.add_node network 0 (InnovationNumber 4)

    Expect.equal network.genome[0].enabled false "enabled"
    Expect.equal network.genome[4].in_node.id.int 0 "in_node1"
    Expect.equal network.genome[4].out_node.id.int 4 "out_node1"
    Expect.equal network.genome[4].weight 1. "weight"
    Expect.equal network.genome[5].in_node.id.int 4 "in_node2"
    Expect.equal network.genome[5].out_node.id.int 2 "out_node2"
    Expect.floatClose Accuracy.high network.genome[5].weight existing_conn_weight "weight"
    Expect.equal network.hidden_nodes.Count 1 "hidden_nodes"
}

let can_add_valid_connection = test "can_add_valid_connection" {
    let r = System.Random()
    let network = Network.init r 2 2
    let network, global_innvation = Network.add_node network 0 (InnovationNumber 4)
    let network, _ = add_connection_by_int network 1 4 0.5 global_innvation
    let network = network |> Network.set_phenotype
    Expect.equal network.output_nodes[1].inputs.Length 2 ""
    Expect.equal network.genome[6].in_node.id.int 1 ""
    Expect.equal network.genome[6].out_node.id.int 4 ""
}

module Network =
    let empty n_sensor_nodes n_output_nodes = 
        let sensor_nodes = Array.init n_sensor_nodes (fun i -> SensorNode(id = NodeId i, value = 0.)) |> ResizeArray
        let output_nodes = Array.init n_output_nodes (fun i -> RelayNode.create_output (i + n_sensor_nodes)) |> ResizeArray
        {
            
            genome = ResizeArray()
            sensor_nodes = sensor_nodes
            output_nodes = output_nodes
            hidden_nodes = ResizeArray()
            node_lookup = [| yield! sensor_nodes |> Seq.map Node.Cast; yield! output_nodes |> Seq.map Node.Cast|] |> ResizeArray
        }

let feed_forward = test "feed forward" {
    let network = Network.empty 2 2
    let global_innov = (InnovationNumber 0)

    let network, global_innov = add_connection_by_int network 0 3 0.6 global_innov
    let network, global_innov = add_connection_by_int network 1 3 -0.9 global_innov
    let network, global_innov = add_node_by_in_out network 0 3 -0.1 global_innov
    let network, global_innov = add_node_by_in_out network 1 3 -0.8 global_innov
    let network, global_innov = add_connection_by_int network 0 5 0.6 global_innov
    let network, global_innov = add_connection_by_int network 5 2 0.4 global_innov
    let network = network |> Network.set_phenotype 
    let new_network = network |> Network.activate[|0.5; -0.2|]
    Expect.floatClose Accuracy.high new_network.output_nodes[0].value 0.184 ""
    Expect.floatClose Accuracy.high new_network.output_nodes[1].value 0. ""
    Expect.equal global_innov (InnovationNumber 8) ""
}

let recurrent = test "recurrent" {
    let network = Network.empty 2 1
    let global_innov = (InnovationNumber 0)

    let network, global_innov = add_connection_by_int network 1 2 0.9 global_innov
    let network, global_innov = add_node_by_in_out network 1 2 0.1 global_innov //this creates node 3 between 1 and 2
    let network, global_innov = add_node_by_in_out network 1 3 -0.8 global_innov //this creates node 4 between 1 and 3
    let network, global_innov = add_connection_by_int network 0 2 -0.4 global_innov
    let network, global_innov = add_node_by_in_out network 0 2 0.0 global_innov //this create node 5 between 0 and 2
    let network = diable_connection_by_in_out network 0 5
    let network, global_innov = add_connection_by_int network 0 4 -0.8 global_innov
    let network, global_innov = add_connection_by_int network 3 5 0.5 global_innov
    let network, global_innov = add_connection_by_int network 5 4 -0.1 global_innov
    
    let network = network |> Network.set_phenotype
    let new_network = network |> Network.activate [|-0.9; 0.6|]
    Expect.floatClose Accuracy.high new_network.output_nodes[0].value 0.0216 ""
}
let large_init = test "large init" {
    let r = System.Random()
    let network = Network.init r 9 10

    Expect.equal network.genome[89].innovation (InnovationNumber 89) ""
    Expect.equal network.genome.Count 90 ""
}

let all_tests = 
    testList ""
        [
            network_creation
            cannot_add_connection_out_to_sensor
            cannot_add_connection_in_from_output
            cannot_add_connection_beyond_length
            cannot_add_preexisting_connection
            can_add_valid_connection
            can_add_valid_node
            large_init
            feed_forward
            recurrent
        ]

[<EntryPoint>]
let main args =
    runTestsWithCLIArgs [] [||] all_tests