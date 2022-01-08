/// An entity that welcomes newbies and give a role or says goodbye to the leavers
module Doorkeeper.Main
open Types
open Model

let settings: Settings =
    [
        927554008263032832UL, [|
            // TODO
        |]
        878956153537691658UL, [|
            929072703728676864UL
        |]
    ] |> Map.ofList

let handle (e: DSharpPlus.EventArgs.GuildMemberAddEventArgs) =
    match Map.tryFind e.Guild.Id settings with
    | Some roleIds ->
        roleIds
        |> Array.iter (fun roleId ->
            match e.Guild.Roles.[roleId] with
            | null -> ()
            | role ->
                try
                    e.Member.GrantRoleAsync(role)
                    |> fun x -> x.GetAwaiter() |> fun x -> x.GetResult()
                with e ->
                    printfn "%A" e.Message
        )
    | None -> ()
