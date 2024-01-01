namespace neat

[<Measure>] type InputNode
[<Measure>] type OutputNode
[<Measure>] type InnovationNumber

type InputNodeId = int<InputNode>
type OutputNodeId = int<OutputNode>
type InnovationNumberId = int<InnovationNumber>

type Connection = {
    in_node_id: InputNodeId
    out_node_id: OutputNodeId
    weight: float
    innovation_num: InnovationNumberId
    enabled: bool
}

type Network = {
    n_sensor_nodes: int
    n_output_nodes: int
    n_hidden_nodes: int
    genome: Connection array
    input_connections_map: int array array
    nodes_with_active_inputs: bool array
    active_nodes: bool array
    active_sums: float array
    node_values: float array
} with 
    member this.n_total_nodes = this.n_hidden_nodes + this.n_output_nodes = this.n_sensor_nodes
    
module Network =
    let set_phenotype (x:Network) =
        let n_activatable = x.n_output_nodes + x.n_hidden_nodes
        let node_offset = x.n_sensor_nodes

        let conn_map = Array.create n_activatable (ResizeArray<int>())
        x.genome |> Array.iteri (fun conn_index conn ->
            conn_map[int conn.out_node_id - node_offset].Add(conn_index)
        )

        {x with input_connections_map = conn_map |> Array.map (fun ra -> ra.ToArray())}
        
    let empty n_sensor_nodes n_output_nodes = {
        n_sensor_nodes = n_sensor_nodes
        n_output_nodes = n_output_nodes
        n_hidden_nodes = 0
        genome = Array.empty
        input_connections_map = Array.empty
        active_nodes = Array.init (n_sensor_nodes + n_output_nodes) (fun i -> i < n_sensor_nodes)
        nodes_with_active_inputs = Array.create n_output_nodes false
        active_sums = Array.create n_output_nodes 0.
        node_values = Array.create (n_sensor_nodes + n_output_nodes) 0.
    }

    let create_from_genome n_sensor_nodes n_output_nodes genome =
        let hidden_index_start = n_sensor_nodes + n_output_nodes
        let n_hidden_nodes =
            genome |> 
            Array.collect (fun (conn:Connection) -> [|
                if int conn.in_node_id >= hidden_index_start then int conn.in_node_id
                if int conn.out_node_id >= hidden_index_start then int conn.out_node_id
            |]) |> Array.distinct |> Array.length
        let n_non_sensor = n_output_nodes + n_hidden_nodes
        let n_total_nodes = n_non_sensor + n_sensor_nodes
        {
            n_sensor_nodes = n_sensor_nodes
            n_output_nodes = n_output_nodes
            n_hidden_nodes = n_hidden_nodes
            genome = genome
            input_connections_map = Array.empty
            active_nodes = Array.init n_total_nodes (fun i -> i < n_sensor_nodes)
            nodes_with_active_inputs = Array.create n_non_sensor false
            active_sums = Array.create n_non_sensor 0.
            node_values = Array.create n_total_nodes 0.
        }