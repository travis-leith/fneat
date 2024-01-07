namespace neat

type InnovationNumber = InnovationNumber of int
type NodeId = NodeId of int with member this.int = let (NodeId i) = this in i

[<AbstractClass>]
type Node(id: NodeId, value: float, is_active: bool) = 
    member val id = id
    member val value = value with get, set
    member val is_active = is_active with get, set
    static member inline Cast (x) = x :> Node
and SensorNode(id: NodeId, value: float) =
    inherit Node(id, value, true)
and RelayNode(id: NodeId, has_active_inputs : bool, inputs : Connection array, is_active : bool, is_output: bool, active_sum : float, value : float) =
    inherit Node(id, value, is_active)
    member val has_active_inputs = has_active_inputs with get, set
    member val inputs = inputs with get, set
    member val is_output = is_output with get, set
    member val active_sum = active_sum with get, set
and Connection(in_node: Node, out_node: RelayNode, weight: float, innovation: InnovationNumber) =
    member val in_node = in_node
    member val out_node = out_node
    member val weight = weight with get, set
    member val innovation = innovation
    member val enabled = true with get, set

module RelayNode =
    let create_output i = RelayNode(
        id = NodeId i
        ,has_active_inputs = false
        ,inputs = Array.empty
        ,is_active  = false
        ,is_output = true
        ,active_sum = 0.
        ,value  = 0.
    )
type Network = {
    genome: Connection ResizeArray
    sensor_nodes: SensorNode ResizeArray
    output_nodes: RelayNode ResizeArray
    hidden_nodes: RelayNode ResizeArray
    node_lookup: Node ResizeArray
}   
module ResizeArray =
    type AllignedTuple<'a> =
        |HasBoth of 'a * 'a
        |HasLeft of 'a
        |HasRight of 'a

    let rec private allign_inner (ar1: _ ResizeArray) (ar2: _ ResizeArray) n1 n2 get_id f i1 i2 (output: _ ResizeArray) =
        let ctrl =
            if i1 < n1 then
                if i2 < n2 then
                    //still processing ar1 and ar2
                    let x1 = ar1[i1]
                    let x2 = ar2[i2]
                    if get_id x1 = get_id x2 then
                        let res = (x1, x2) |> HasBoth |> f, i1 + 1, i2 + 1
                        Some res
                    elif get_id x1 < get_id x2 then
                        let res = x1 |> HasLeft |> f, i1 + 1, i2
                        Some res
                    else
                        let res = x2 |> HasRight |> f, i1, i2 + 1
                        Some res
                else
                    //still processing ar1 but finished with ar2
                    let x1 = ar1[i1]
                    let res = x1 |> HasLeft |> f, i1 + 1, i2
                    Some res
            elif i2 < n2 then
                //finished processing ar1 but still busy with ar2
                let x2 = ar2[i2]
                let res = x2 |> HasRight |> f, i1, i2 + 1
                Some res
            else
                None

        match ctrl with
        |Some (out, i1_, i2_) ->
            output.Add out
            allign_inner ar1 ar2 n1 n2 get_id f i1_ i2_ output
        |None -> output

    let allign ar1 ar2 get_id f =
        let out = ResizeArray()
        allign_inner ar1 ar2 ar1.Count ar2.Count get_id f 0 0 out

    let prefer_left ar1 ar2 get_id =
        let f = function
            |HasBoth (a, _)| HasLeft a -> a
            |HasRight a -> a
        allign ar1 ar2 get_id f
    
module Network =
    let set_phenotype (x:Network) =

        let y =
            x.genome 
            |> Seq.sortBy (fun conn -> conn.out_node.is_output, conn.out_node.id)
            |> Seq.groupBy (_.out_node.is_output)
            |> Seq.map (fun (is_output, sub_seq) -> 
                is_output, sub_seq 
                |> Seq.groupBy (_.out_node.id)
                |> Seq.map (fun (_, sub_sub_ar) ->
                    let conn = Seq.head sub_sub_ar
                    let out_node = conn.out_node
                    out_node.inputs <- sub_sub_ar |> Seq.toArray
                    out_node
                )
                |> ResizeArray
            )
            |> List.ofSeq
            
        let hidden_nodes, output_nodes =
            match y with
            |[hidden; output] -> snd hidden, snd output
            |[output]-> ResizeArray(), snd output
            |_ -> failwith "unreachable"

        //make sure all outputs are accounted for
        let output_nodes =
            if x.output_nodes.Count = output_nodes.Count then
                output_nodes
            else
                ResizeArray.prefer_left output_nodes x.output_nodes _.id

        let node_lookup =
            [|
                yield! x.sensor_nodes |> Seq.map Node.Cast
                yield! x.output_nodes |> Seq.map Node.Cast
                yield! x.hidden_nodes |> Seq.map Node.Cast
            |] |> ResizeArray
        {x with hidden_nodes = hidden_nodes |> ResizeArray; output_nodes = output_nodes; node_lookup = node_lookup}
        
    let create_from_genome n_sensor_nodes n_output_nodes (genome: Connection ResizeArray) =
        let sensor_nodes = //could probably derive this from the first n_sensor_nodes of genome
            genome
            |> Seq.take n_sensor_nodes
            |> Seq.map (fun conn -> conn.in_node :?> SensorNode)
            |> ResizeArray

        if (sensor_nodes |> Seq.distinctBy (fun n -> n.id) |> Seq.length) <> n_sensor_nodes then
            failwith "assumption that sensor nodes can be derived from genome is incorrect"

        let sensor_nodes =
            if sensor_nodes.Count = n_sensor_nodes then
                sensor_nodes
            else
                ResizeArray.prefer_left sensor_nodes (Array.init n_sensor_nodes (fun i -> SensorNode(NodeId i, 0.)) |> ResizeArray) _.id

        let output_nodes = Array.init n_output_nodes (fun i -> RelayNode.create_output (i + n_sensor_nodes)) |> ResizeArray
        {
            genome = genome |> ResizeArray
            sensor_nodes = sensor_nodes
            output_nodes = output_nodes
            hidden_nodes = ResizeArray() //get set durng set_phenotype
            node_lookup = ResizeArray() //get set durng set_phenotype
        } |> set_phenotype

    let init r n_sensor_nodes n_output_nodes =
        let sensor_nodes = Array.init n_sensor_nodes (fun i -> SensorNode(NodeId i, 0.)) |> ResizeArray
        let output_nodes =  Array.init n_output_nodes (fun i -> RelayNode.create_output (i + n_sensor_nodes)) |> ResizeArray
        
        let connections =
            [|
                for out_index = 0 to n_output_nodes - 1 do
                    let these_connections = 
                        [|
                            for in_index = 0 to n_sensor_nodes - 1 do
                                let innovation_number = out_index * n_sensor_nodes + in_index
                                Connection(
                                    in_node = (sensor_nodes[in_index] :> Node)
                                    ,out_node = output_nodes[out_index]
                                    ,weight = Random.weight r
                                    ,innovation = (innovation_number |> InnovationNumber)
                                )
                        |]
                    output_nodes[out_index].inputs <- these_connections
                    yield! these_connections
            |]
            
        let node_lookup =
            [|
                yield! sensor_nodes |> Seq.map Node.Cast
                yield! output_nodes |> Seq.map Node.Cast
            |] |> ResizeArray

        {
            genome = connections |> ResizeArray
            sensor_nodes = sensor_nodes
            output_nodes = output_nodes
            hidden_nodes = ResizeArray()
            node_lookup = node_lookup
        }

    let relu x = max x 0.

    let activation_pulse (network:Network) =
        printf "start active list: "
        network.node_lookup |> Seq.iteri (fun i node -> printf $"({i}, {node.is_active})")
        printfn ""
        let accumulate_node_array_inputs (ar: ResizeArray<RelayNode>) =
            for i = 0 to ar.Count - 1 do
                ar[i].has_active_inputs <- false
                ar[i].active_sum <- 0.
                for conn in ar[i].inputs do
                    if conn.in_node.is_active && conn.enabled then
                        let to_add = conn.weight * conn.in_node.value
                        ar[i].has_active_inputs <- true
                        ar[i].active_sum <- ar[i].active_sum + to_add

        accumulate_node_array_inputs network.hidden_nodes
        printf "post hidden acc active list: "
        network.node_lookup |> Seq.iteri (fun i node -> printf $"({i}, {node.is_active})")
        printfn ""
        accumulate_node_array_inputs network.output_nodes
        printf "post output acc active list: "
        network.node_lookup |> Seq.iteri (fun i node -> printf $"({i}, {node.is_active})")
        printfn ""

        let mutable all_active = true
            
        let activate_nodes (ar: ResizeArray<RelayNode>) =
            for i = 0 to ar.Count - 1 do
                ar[i].value <- relu ar[i].active_sum //TODO: should this be inside the if?
                if ar[i].has_active_inputs then
                    printfn $"setting ar[{i}].is_active <- true when it currently is {ar[i].is_active} | nodeId: {ar[i].id.int}"
                    ar[i].is_active <- true
                else
                    all_active <- false
                    
        activate_nodes network.hidden_nodes
        printf "post hidden actv active list: "
        network.node_lookup |> Seq.iteri (fun i node -> printf $"({i}, {node.is_active})")
        printf " but hidden nodes ->"
        network.hidden_nodes |> Seq.iteri (fun i node -> printf $"({i}, {node.is_active})")
        printfn ""
        activate_nodes network.output_nodes
        printf "post output actv active list: "
        network.node_lookup |> Seq.iteri (fun i node -> printf $"({i}, {node.is_active})")
        printfn ""

        printfn "outputs -> inputs -> active"
        network.output_nodes |> Seq.iteri (fun i node ->
            printf $"i: {i}; nodeId: {node.id.int}; inputs -> "
            node.inputs |> Seq.iter (fun conn ->
                printf $"id: {conn.in_node.id}; active: {conn.in_node.is_active}| "
            )
            printfn ""
        )

        network, all_active

    let activate (sensor_values: float array) (network:Network) = 
        for i = 0 to network.sensor_nodes.Count - 1 do
            network.sensor_nodes[i].value <- sensor_values[i]

        let rec activate_inner current_network remaining_iterations =
            printfn $"remaining iters {remaining_iterations}"
            if remaining_iterations = 0 then
                failwith "Too many iterations :("
            else
                let new_network, all_activated = activation_pulse current_network
                if all_activated then
                    new_network
                else
                    activate_inner new_network (remaining_iterations - 1)

        activate_inner network 20

    let increment_innovation x = let (InnovationNumber i) = x in (i + 1) |> InnovationNumber
    let add_connection (network: Network) (in_id:NodeId) (out_id:NodeId) weight global_innovation =
        if out_id.int > network.node_lookup.Count then
            failwith $"Tried to add a connection with an output ({out_id.int}) that skips an available id ({network.node_lookup.Count})"

        let out_node =
            match network.node_lookup[out_id.int] with
            | :? RelayNode as node when node.inputs |> Seq.exists (fun conn -> conn.in_node.id = in_id) ->
                failwith "Tried to connect 2 ndoes that are already connected"
            | :? RelayNode as node -> node
            |_ -> failwith "Tried to add a connection that outputs to a sensor node"

        if in_id.int >= network.sensor_nodes.Count && in_id.int < (network.sensor_nodes.Count + network.output_nodes.Count) then
            failwith "Tried to add a connection that inputs from an output node"

        let new_conn = Connection(
            in_node = network.node_lookup[in_id.int]
            ,out_node = out_node
            ,weight = weight
            ,innovation = global_innovation
        )
        network.genome.Add(new_conn)
        network, increment_innovation global_innovation

    let add_node (network: Network) existing_conn_index global_innovation =
        if not network.genome[existing_conn_index].enabled then
            failwith "Tried to add a node to a disabled connection"

        let new_node_id = NodeId network.node_lookup.Count
        let new_relay_node = RelayNode(
            id = new_node_id
            ,has_active_inputs = false
            ,inputs = Array.empty //TODO
            ,is_active = false
            ,is_output = false
            ,active_sum = 0.
            ,value = 0.
        )
        
        network.genome[existing_conn_index].enabled <- false

        let conn_in_id = network.genome[existing_conn_index].in_node.id
        let conn_out_id = network.genome[existing_conn_index].out_node.id
        let conn_weight = network.genome[existing_conn_index].weight

        network.hidden_nodes.Add new_relay_node
        network.node_lookup.Add (new_relay_node :> Node)
        let new_network, global_innovation = add_connection network conn_in_id new_node_id 1. global_innovation
        add_connection new_network new_node_id conn_out_id conn_weight global_innovation

type Organism = {
    network: Network
    fitness: float
}

module Organism =
    let init r n_sensor_nodes n_output_nodes =
        {network = Network.init r n_sensor_nodes n_output_nodes; fitness = 0.}

    let activate sensor_values (organism:Organism)  =
        {organism with network = Network.activate sensor_values organism.network}

    let allign_genes (organism_1:Organism) (organism_2:Organism) =
        let genome_size_1 = organism_1.network.genome.Count
        let genome_size_2 = organism_2.network.genome.Count

        let rec get_gene_pairs gene_index_1 gene_index_2 acc =
            if gene_index_1 < genome_size_1 then
                if gene_index_2 < genome_size_2 then
                    //still processing org_1 and org_2
                    let conn1 = organism_1.network.genome[gene_index_1]
                    let conn2 = organism_2.network.genome[gene_index_2]
                    if conn1.innovation = conn2.innovation then
                        let gene_pair = Some conn1, Some conn2
                        get_gene_pairs (gene_index_1 + 1) (gene_index_2 + 1) (gene_pair::acc)
                    elif conn1.innovation < conn2.innovation then
                        let gene_pair = Some conn1, None
                        get_gene_pairs (gene_index_1 + 1) gene_index_2 (gene_pair::acc)
                    else
                        let gene_pair = None, Some conn2
                        get_gene_pairs gene_index_1 (gene_index_2 + 1) (gene_pair::acc)
                else
                    //still processing org_1 but finished with org_2
                    let conn1 = organism_1.network.genome[gene_index_1]
                    let gene_pair = Some conn1, None
                    get_gene_pairs (gene_index_1 + 1) gene_index_2 (gene_pair::acc)
            elif gene_index_2 < genome_size_2 then
                //finished processing org_1 but still busy with org_2
                let conn2 = organism_2.network.genome[gene_index_2]
                let gene_pair = None, Some conn2
                get_gene_pairs gene_index_1 (gene_index_2 + 1) (gene_pair::acc)
            else
                acc
        ResizeArray.allign organism_1.network.genome organism_2.network.genome (fun x -> x.innovation) id