module ReactVehSearch

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

// JS utility for conditionally joining classNames together
let classNames(classes: obj): string =
    importDefault "./lib/classnames.js"

// Director is a router. Routing is the process of determining
// what code to run when a URL is requested.
let Router(routes: obj): obj =
    importDefault "./lib/director.js"

//load and save data from the browser local storage as things like Guid generation
module Util =
    let load<'T> key: 'T option =
        !!Browser.localStorage.getItem(key)
        |> Option.map (fun json -> !!JS.JSON.parse(json))

    let save key (data: 'T) =
        Browser.localStorage.setItem(key, JS.JSON.stringify data)

// Model
type Brand = { id: int; label: string }
type Make = { id: int; brandId: int; label: string }
type Search = { id: Guid; brandId: int; makeId: int option}

type VehicleModel(key) =
    member val key = key
    member val search: Search option = defaultArg (Util.load key) None with get, set
    member val onChanges: (unit->unit)[] = [||] with get, set

    member this.subscribe(onChange) =
        this.onChanges <- [|onChange|]


module R = Fable.Helpers.React
open R.Props

// Vehicle Search view app
type [<Pojo>] VehicleSearchAppProps =
    { model: VehicleModel }

type [<Pojo>] VehicleSearchAppState =
    { currentStep: string
    ; searching: Guid option }

type VehicleSearchApp(props) =
    inherit React.Component<VehicleSearchAppProps, VehicleSearchAppState>(props)
    do base.setInitState({ currentStep = "brand"; searching = None })

    member this.render () =
        R.section [ ClassName "main" ] [
            R.ul [ ClassName "todo-list" ] "From Fable"
        ] |> Some

// Firing up the app
let model = VehicleModel("react-veh-search")
let render() =
    ReactDom.render(
        R.com<VehicleSearchApp,_,_> { model = model } [],
        Browser.document.getElementsByClassName("container").[0]
    )
model.subscribe(render)
render()