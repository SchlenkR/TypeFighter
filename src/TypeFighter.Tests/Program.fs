module TypeFighter.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    runTestsWithCLIArgs [] args TestCases.tests
    
    //let logary =
    //Config.create "MyProject.Tests" "localhost"
    //|> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
    //|> Config.processing (Events.events |> Events.sink ["console";])
    //|> Config.build
    //|> run
    //LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary

    //// Invoke Expecto:
    //runTestsInAssemblyWithCLIArgs [] argv
