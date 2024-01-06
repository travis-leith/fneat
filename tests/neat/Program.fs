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
    let net, inn = Network.add_connection network (inputNodeId in_id) (outputNodeId out_id) weight global_innovation
    net, inn

let add_node_by_in_out (network:Network) in_id out_id new_weight global_innovation =
    let conn_index =
        match network.genome |> Seq.tryFindIndex (fun x -> int x.in_node_id = in_id && int x.out_node_id = out_id && x.enabled) with
        |Some i -> i
        |None -> failwith "Cannot add a node to a connection that does not exist"

    let new_conn_index = network.genome.Count
    let new_network, global_innov = Network.add_node network conn_index global_innovation
    new_network.genome[new_conn_index] <- new_network.genome[new_conn_index].WithWeight new_weight
    new_network, global_innov

let diable_connection_by_in_out (network:Network) in_id out_id =
    let conn_index =
        match network.genome |> Seq.tryFindIndex (fun x -> int x.in_node_id = in_id && int x.out_node_id = out_id && x.enabled) with
        |Some i -> i
        |None -> failwith "Cannot disable a connection that does not exist"

    network.genome[conn_index] <- network.genome[conn_index].WithEnabled false
    network

let network_creation = test "network creation" {
    let n_sensors = 3
    let n_outputs = 2
    let n_total = n_sensors + n_outputs
    let network = Network.empty n_sensors n_outputs |> Network.set_phenotype

    Expect.equal network.input_connections_map.Length n_outputs ""
    Expect.equal network.active_nodes.Count n_total ""
    Expect.equal network.nodes_with_active_inputs.Count n_outputs ""
    Expect.equal network.active_sums.Count n_outputs ""
    Expect.equal network.node_values.Count n_total ""
}

let cannot_add_connection_out_to_sensor = test "cannot_add_connection_out_to_sensor" {
    let network = Network.empty 2 2
    Expect.exn_message (fun () -> add_connection_by_int network 1 0 0. (innovationNumberId 0) |> ignore) "Tried to add a connection that outputs to a sensor node"
}

let cannot_add_connection_in_from_output = test "cannot_add_connection_in_from_output" {
    let network = Network.empty 2 2
    Expect.exn_message (fun () -> add_connection_by_int network 2 3 0. (innovationNumberId 0) |> ignore) "Tried to add a connection that inputs from an output node"
}

let cannot_add_connection_beyond_length = test "cannot_add_connection_beyond_length" {
    let network = Network.empty 2 2
    Expect.exn_message (fun () -> add_connection_by_int network 1 5 0. (innovationNumberId 0) |> ignore) "Tried to add a connection with an output (5) that skips an available id (4)"
}

let can_add_valid_connection = test "can_add_valid_connection" {
    let network = Network.empty 2 2
    let network, _ = add_connection_by_int network 1 2 0.5 1<InnovationNumber>
    let network = network |> Network.set_phenotype
    Expect.equal network.input_connections_map[0].Length 1 ""
    let conn_index = network.input_connections_map[0][0]
    Expect.equal network.genome[conn_index].in_node_id 1<InputNode> ""
    Expect.equal network.genome[conn_index].innovation_num 1<InnovationNumber> ""
    Expect.equal network.nodes_with_active_inputs.Count 2 ""
}

let can_add_valid_node = test "can_add_valid_node" {
    let network = Network.empty 2 2
    let network, global_innvation = add_connection_by_int network 1 2 0.5 1<InnovationNumber>
    let network, global_innvation = Network.add_node network 0 global_innvation

    Expect.equal network.genome[0].enabled false ""
    Expect.equal network.genome[1].in_node_id 1<InputNode> ""
    Expect.equal network.genome[1].out_node_id 4<OutputNode> ""
    Expect.equal network.genome[1].weight 1. ""
    Expect.equal network.genome[2].in_node_id 4<InputNode> ""
    Expect.equal network.genome[2].out_node_id 2<OutputNode> ""
    Expect.equal network.genome[2].weight 0.5 ""
    Expect.equal network.nodes_with_active_inputs.Count 3 ""
}

let feed_forward = test "feed forward" {
    let network = Network.empty 2 2
    let global_innov = 0<InnovationNumber>

    let network, global_innov = add_connection_by_int network 0 3 0.6 global_innov
    let network, global_innov = add_connection_by_int network 1 3 -0.9 global_innov
    let network, global_innov = add_node_by_in_out network 0 3 -0.1 global_innov
    let network, global_innov = add_node_by_in_out network 1 3 -0.8 global_innov
    let network, global_innov = add_connection_by_int network 0 5 0.6 global_innov
    let network, global_innov = add_connection_by_int network 5 2 0.4 global_innov
    let network = network |> Network.set_phenotype 
    let new_network = network |> Network.activate[|0.5; -0.2|]
    Expect.floatClose Accuracy.high new_network.node_values[2] 0.184 ""
    Expect.floatClose Accuracy.high new_network.node_values[3] 0. ""
    Expect.equal (int global_innov) 8 ""
}

let recurrent = test "recurrent" {
    let network = Network.empty 2 1
    let global_innov = 0<InnovationNumber>

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
    Expect.floatClose Accuracy.high new_network.node_values[2] 0.0216 ""
}
let large_init = test "large init" {
    let r = System.Random()
    let network = Network.init r 9 10

    Expect.equal network.genome[89].innovation_num 89<InnovationNumber> ""
    Expect.equal network.genome.Count 90 ""
}

let all_tests = 
    testList ""
        [
            network_creation
            cannot_add_connection_out_to_sensor
            cannot_add_connection_in_from_output
            cannot_add_connection_beyond_length
            can_add_valid_connection
            can_add_valid_node
            large_init
            feed_forward
            recurrent
        ]

[<EntryPoint>]
let main args =
    runTestsWithCLIArgs [] [||] all_tests