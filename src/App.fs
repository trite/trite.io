module App

open Elmish
open Elmish.React
open Feliz
open Fable.SimpleHttp

let testUri =
    "https://raw.githubusercontent.com/trite/trite.io-blog/main/1999/01/1999-01-01_time-to-start-a-blog-stub.md"

type State =
    {
        Count : int
        FetchedContent : string
    }

type FetchStatus =
    | Started
    | Finished of result : string

type Msg =
    | Increment
    | Decrement
    | FetchPage of FetchStatus

let init () =
    {
        Count = 0
        FetchedContent = ""
    },
    Cmd.none

let fromAsync (operation : Async<'msg>) : Cmd<'msg> =

    let delayedCmd (dispatch : 'msg -> unit) : unit =

        let delayedDispatch = async {
            let! msg = operation
            dispatch msg
        }

        Async.StartImmediate delayedDispatch

    Cmd.ofSub delayedCmd

let update (msg: Msg) (state: State) =
    let addToCount x state =
        { state with Count = state.Count + x }, Cmd.none

    match msg with
    | Increment ->
        addToCount 6 state

    | Decrement ->
        addToCount -13 state

    | FetchPage Started ->
        let fetchedContent =
            async {
                let! (statusCode, responseText) =
                    Http.get testUri

                match statusCode with
                | 200 -> return FetchPage (Finished responseText)
                | _   -> return FetchPage (Finished "Something went wrong!")
            }

        let nextState =
            { state with FetchedContent = "Fetching..." }
        
        nextState, fromAsync fetchedContent

    | FetchPage (Finished result) ->
        let nextState =
            { state with FetchedContent = result }
        nextState, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        Html.div [
            Html.button [
                prop.style [
                    style.padding 5
                    style.margin 5
                ]
                prop.onClick (fun _ -> dispatch Increment)
                prop.text "Make it go ^"
            ]

            Html.button [
                prop.style [
                    style.padding 10
                    style.margin 10
                ]
                prop.onClick (fun _ -> dispatch Decrement)
                prop.text "Make it go v"
            ]

            Html.h1 state.Count
        ]
        Html.div [
            Html.button [
                prop.style []
                prop.onClick (fun _ -> dispatch (FetchPage Started))
                prop.text "Fetch the page"
            ]

            Html.pre state.FetchedContent
        ]
    ]

// Program.mkSimple init update render
Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run