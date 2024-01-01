open Expecto
open neat
let test_network_creation = test "network_creation" {
    let n_sensors = 3
    let n_outputs = 2
    let n_total = n_sensors + n_outputs
    let network = Network.empty n_sensors n_outputs |> Network.set_phenotype

    Expect.equal network.input_connections_map.Length n_outputs ""
    Expect.equal network.active_nodes.Length n_total ""
    Expect.equal network.nodes_with_active_inputs.Length n_outputs ""
    Expect.equal network.active_sums.Length n_outputs ""
    Expect.equal network.node_values.Length n_total ""
}

[<EntryPoint>]
let main args =
    runTestsWithCLIArgs [] [||] test_network_creation