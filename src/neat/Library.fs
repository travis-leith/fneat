namespace neat

[<Measure>] type InputNode
[<Measure>] type OutputNode
[<Measure>] type InnovationNumber

type InputNodeId = int<InputNode>
type OutputNodeId = int<OutputNode>
type InnovationNumberId = int<InnovationNumber>

[<AutoOpen>]
module Units =
    let inputNodeId x = x * 1<InputNode>
    let outputNodeId x = x * 1<OutputNode>
    let innovationNumberId x = x * 1<InnovationNumber>

type Connection = {
    in_node_id: InputNodeId
    out_node_id: OutputNodeId
    weight: float
    innovation_num: InnovationNumberId
    enabled: bool
} with
    member this.WithWeight f = {this with weight = f}
    member this.WithEnabled b = {this with enabled = b}

type Network = {
    n_sensor_nodes: int
    n_output_nodes: int
    n_hidden_nodes: int
    genome: Connection ResizeArray
    input_connections_map: int array array
    nodes_with_active_inputs: bool ResizeArray
    active_nodes: bool ResizeArray
    active_sums: float ResizeArray
    node_values: float ResizeArray
} with 
    member this.n_total_nodes = this.n_hidden_nodes + this.n_output_nodes + this.n_sensor_nodes
    member this.with_node_values x = {this with node_values = x}
    
    
module Network =
    let set_phenotype (x:Network) =
        let n_activatable = x.n_output_nodes + x.n_hidden_nodes
        let node_offset = x.n_sensor_nodes

        let conn_map = Array.init n_activatable (fun _ -> ResizeArray<int>())
        x.genome |> Seq.iteri (fun conn_index conn ->
            let map_index = int conn.out_node_id - node_offset
            conn_map[map_index].Add(conn_index)
        )

        {x with input_connections_map = conn_map |> Array.map (fun ra -> ra.ToArray())}
        
    let empty n_sensor_nodes n_output_nodes = {
        n_sensor_nodes = n_sensor_nodes
        n_output_nodes = n_output_nodes
        n_hidden_nodes = 0
        genome = ResizeArray()
        input_connections_map = Array.empty
        active_nodes = Array.init (n_sensor_nodes + n_output_nodes) (fun i -> i < n_sensor_nodes) |> ResizeArray
        nodes_with_active_inputs = Array.create n_output_nodes false|> ResizeArray
        active_sums = Array.create n_output_nodes 0.|> ResizeArray
        node_values = Array.create (n_sensor_nodes + n_output_nodes) 0.|> ResizeArray
    }

    let create_from_genome n_sensor_nodes n_output_nodes genome =
        let hidden_index_start = n_sensor_nodes + n_output_nodes
        let n_hidden_nodes =
            genome |> 
            Seq.collect (fun (conn:Connection) -> [|
                if int conn.in_node_id >= hidden_index_start then int conn.in_node_id
                if int conn.out_node_id >= hidden_index_start then int conn.out_node_id
            |]) |> Seq.distinct |> Seq.length
        let n_non_sensor = n_output_nodes + n_hidden_nodes
        let n_total_nodes = n_non_sensor + n_sensor_nodes
        {
            n_sensor_nodes = n_sensor_nodes
            n_output_nodes = n_output_nodes
            n_hidden_nodes = n_hidden_nodes
            genome = genome
            input_connections_map = Array.empty
            active_nodes = Array.init n_total_nodes (fun i -> i < n_sensor_nodes) |> ResizeArray
            nodes_with_active_inputs = Array.create n_non_sensor false |> ResizeArray
            active_sums = Array.create n_non_sensor 0. |> ResizeArray
            node_values = Array.create n_total_nodes 0. |> ResizeArray
        }

    let init r n_sensor_nodes n_output_nodes =
        let connections = 
            [|
                for in_id in 0 .. n_sensor_nodes - 1 do
                    for out_id in n_sensor_nodes .. (n_sensor_nodes + n_output_nodes) - 1 do
                        let innovation_number = in_id * n_output_nodes + out_id - n_sensor_nodes
                        {
                            in_node_id = in_id |> inputNodeId
                            out_node_id = out_id |> outputNodeId
                            weight = Random.weight r
                            innovation_num = innovation_number |> innovationNumberId
                            enabled = true
                        }
            |]

        {empty n_sensor_nodes n_output_nodes with genome = ResizeArray connections}
        |> set_phenotype

    let relu x = max x 0.

    //consider optimizing the data structure for activation. make the phenotype more nested with links?
    //use immutable collections for the genome and phenome and arrays for the others?
    let activation_pulse (network:Network) =
        network.input_connections_map |> Array.iteri (fun node_index_offset input_connections ->
            network.nodes_with_active_inputs[node_index_offset] <- false
            network.active_sums[node_index_offset] <- 0.

            input_connections |> Array.iter (fun conn_id ->
                let conn = network.genome[conn_id]
                if network.active_nodes[int conn.in_node_id] && conn.enabled then
                    network.nodes_with_active_inputs[node_index_offset] <- true
                    let to_add = conn.weight * network.node_values[int conn.in_node_id]
                    network.active_sums[node_index_offset] <- network.active_sums[node_index_offset] + to_add
                ()
            )
        )

        let mutable all_active = true

        for node_index_offset in 0 .. network.nodes_with_active_inputs.Count - 1 do
            let node_index = node_index_offset + network.n_sensor_nodes
            network.node_values[node_index] <- relu network.active_sums[node_index_offset]
            if network.nodes_with_active_inputs[node_index_offset] then
                network.active_nodes[node_index] <- true
            else
                all_active <- false
                
        network, all_active
    let activate (sensor_values: float array) (network:Network) = 
        for i in 0 .. network.n_sensor_nodes - 1 do
            network.node_values[i] <- sensor_values[i]

        let rec activate_inner current_network remaining_iterations =
            if remaining_iterations = 0 then
                failwith "Too many iterations :("
            else
                let new_network, all_activated = activation_pulse current_network
                if all_activated then
                    new_network
                else
                    activate_inner new_network (remaining_iterations - 1)

        activate_inner network 20

    let increment_innovation (x: InnovationNumberId) = let i = int x in (i + 1) |> innovationNumberId
    let add_connection (network: Network) in_id out_id weight global_innovation =
        if int out_id < network.n_sensor_nodes then
            failwith "Tried to add a connection that outputs to a sensor node"

        if int in_id >= network.n_sensor_nodes && int in_id < (network.n_sensor_nodes + network.n_output_nodes) then
            failwith "Tried to add a connection that inputs from an output node"

        if int out_id > network.n_total_nodes then
            failwith $"Tried to add a connection with an output ({out_id}) that skips an available id ({network.n_total_nodes})"

        if network.input_connections_map.Length > 0 then
            failwith "Add connections to a network with a phenotype is not allowed"
            
        let new_conn = {in_node_id = in_id; out_node_id = out_id; weight = weight; innovation_num = global_innovation; enabled = true}
        network.genome.Add(new_conn)
        network, increment_innovation global_innovation

    let add_node (network: Network) existing_conn_index global_innovation =
        if not network.genome[existing_conn_index].enabled then
            failwith "Tried to add a node to a disabled connection"

        let new_node_id = network.n_total_nodes
        network.genome[existing_conn_index] <- {network.genome[existing_conn_index] with enabled = false}
        // let new_n_hidden_nodes = network.n_hidden_nodes + 1
        network.node_values.Add 0.
        network.active_sums.Add 0.
        network.active_nodes.Add false
        network.nodes_with_active_inputs.Add false

        let conn_in_id = network.genome[existing_conn_index].in_node_id
        let conn_out_id = network.genome[existing_conn_index].out_node_id
        let conn_weight = network.genome[existing_conn_index].weight

        let new_network, global_innovation = add_connection network conn_in_id (outputNodeId new_node_id) 1. global_innovation
        let new_network, global_innovation = add_connection new_network (inputNodeId new_node_id) conn_out_id conn_weight global_innovation
        {new_network with n_hidden_nodes = network.n_hidden_nodes + 1}, global_innovation

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
                    if conn1.innovation_num = conn2.innovation_num then
                        let gene_pair = Some conn1, Some conn2
                        get_gene_pairs (gene_index_1 + 1) (gene_index_2 + 1) (gene_pair::acc)
                    elif conn1.innovation_num < conn2.innovation_num then
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

        get_gene_pairs 0 0 []