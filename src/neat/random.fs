namespace neat

module Random =
    let weight (r:System.Random) =
        r.NextDouble() * 2. - 1.