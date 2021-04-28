
#I @"C:\source\root\Prosim\Cargo\LogisticsDesigner\PlatformService\Host\bin\Debug\net461"
#r "System.ValueTuple.dll"
#r "EntityFramework.dll"
#r "Prosim.Cargo.Commanding.dll"
#r "Prosim.Cargo.Planning.dll"
#r "Prosim.Cargo.Planning.Data.dll"
#r "Prosim.Common.Utils.dll"
#r "Prosim.Common.Logging.dll"
#r "Prosim.Common.DataRepository.dll"
#r "Prosim.Cargo.Commanding.Interfaces.dll"
#r "Prosim.Cargo.SingleRail.Interfaces.dll"
#r "Prosim.Cargo.SingleRail.Model.dll"
#r "Newtonsoft.Json.dll"

module Ndc =

    open System
    open Prosim.Cargo.SingleRail.Model.Factories
    open Prosim.Common.Logging
    
    type DbLogin = { user: string; pw: string }
    
    LoggerFactory.Instance <- ConsoleLoggerFactory()
    
    let private loadContext dbServer (dbLogin: DbLogin) dbName (scenarioId: int) =
        printfn "Loading editor context..."

        let context =
            let baseCs = sprintf @"Data Source=%s;Initial Catalog=%s;Connect Timeout=5;%s" dbServer dbName
            let connectionString = baseCs (sprintf "Integrated Security=false;User ID=%s;Password=%s" dbLogin.user dbLogin.pw)
            let edmConnectionString = sprintf @"metadata=res://*/SingleRailPlanningModel.csdl|res://*/SingleRailPlanningModel.ssdl|res://*/SingleRailPlanningModel.msl;provider=System.Data.SqlClient;provider connection string=""%s""" connectionString
            let logger = LoggerFactory.Instance.Create "default"
            let objectFactory = SingleRailObjectFactory()
            new Prosim.Cargo.Planning.Data.Context.EditorContext(edmConnectionString, logger, scenarioId, objectFactory)
        context.EnsureInfrastructureDataLoaded()
        context.EnsurePlanningDataLoaded()
    
        printfn "Editor context loaded..."
        context
    
    let getEnvVar key =
        let value = Environment.GetEnvironmentVariable key
        if String.IsNullOrWhiteSpace value then
            failwithf "Environment var %s must be set." key
        value

    let loadScenario =
        let password = getEnvVar "NDC_DB_PW"
        let userName = $"dba_" + Environment.UserName.ToLowerInvariant()
        let dbLogin = { user = userName; pw = password }
        //let server = "networkd.cv3hazgbsyhk.eu-central-1.rds.amazonaws.com"
        let server = "127.0.0.1,30100"
        loadContext server dbLogin

let context = Ndc.loadScenario "NDC_Standardangebot_SADe_04-21_20210409" 2677

let prosimAssemblies =
    System.AppDomain.CurrentDomain.GetAssemblies()
    |> Array.filter (fun a -> a.FullName.StartsWith "Prosim")

///////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////


#load "base.fsx"
open Base




(*
    let x = 10.0
    take(
        map c.BlockEdges (be =>
            let id = be.Id
            new 
                { 
                    oldId = id
                    newId = add id x
                }),
        2)
*)

(Let "x" (Num 10.0)
(Appt (Var "take") [
    (Appt (Var "map") [ Prop (Var "context") "BlockEdges"; Abs "be"
        (Let "id" (Prop (Var "be") "Id")
        (Record
            [
                "oldId", (Var "id")
                "newId", Appt (Var "add") [ Var "id"; Var "x" ]
            ] )) ])
    Num 2.0] ))
|> (Compiler.compileAndRun (context, CoreLib.ViewCache()) prosimAssemblies >> printSeq)

