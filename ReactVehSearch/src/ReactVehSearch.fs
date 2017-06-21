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
type SearchStepItem = { id: int; label: string }
type SearchStep =
| Make of SearchStepItem
| Model of SearchStepItem
| Fuel of SearchStepItem
| Transmission of SearchStepItem
| Body of SearchStepItem
| FiscalPower of SearchStepItem

let searchStepDeconstruct searchStep =
    match searchStep with
    | Make i | Model i | Fuel i | Transmission i | Body i | FiscalPower i -> (i.id |> string), i.label
    | _ -> failwith "current search step not found"

type Search = { id: Guid; searchSteps: SearchStepItem list; showResults: bool }

type SearchModel(key) =
    member val key = key
    member val search: Search = defaultArg (Util.load key) {id = System.Guid.NewGuid(); searchSteps = []; showResults = false } with get, set
    member val onChanges: (unit->unit)[] = [||] with get, set

    member this.subscribe(onChange) =
        this.onChanges <- [|onChange|]

    member this.inform() =
        Util.save this.key this.search
        this.onChanges |> Seq.iter (fun cb -> cb())

    member this.select(selectedItem) =
        Browser.console.log(selectedItem)
        this.inform()


module R = Fable.Helpers.React
open R.Props

type [<Pojo>] SearchItemProps =
    { searchStep: SearchStep
    ; step: int 
    ; onSelect: React.SyntheticEvent->unit }

type [<Pojo>] SearchItemState =
    { currentStep: int }

type SearchItem(props) =
    inherit React.Component<SearchItemProps, SearchItemState>(props)
    do base.setInitState({ currentStep = props.step })

    member this.render () =
        let id, label = searchStepDeconstruct this.props.searchStep
        R.a [
            ClassName "list-group-item"
            !!("data-id", id)
            OnClick (fun e -> this.props.onSelect(upcast e)) ] [R.str label]


// Vehicle Search view app
type [<Pojo>] VehicleSearchAppProps =
    { model: SearchModel }

type [<Pojo>] VehicleSearchAppState =
    { currentStep: int
    ; searching: Guid option
    ; searchSteps: SearchStep list }

type VehicleSearchApp(props) =
    inherit React.Component<VehicleSearchAppProps, VehicleSearchAppState>(props)
    do base.setInitState({ currentStep = 0; searching = System.Guid.NewGuid() |> Some; searchSteps = [] })

    member this.componentDidMount () =
        // this could be an ajax call (this is called only once)
        let searchSteps = [Make ({ id = 1; label = "Mercedes"}); Make({ id = 2; label = "BMW" })]
        this.setState({this.state with searchSteps = searchSteps})

    member this.select (searchStep) =
        this.props.model.select(searchStep)
        
    member this.cancel () =
        Browser.console.log("cancelled")
        this.setState({ this.state with currentStep = 0 })

    member this.render () =
        let searchSteps = this.state.searchSteps
        let brandItems =
            searchSteps
            |> Seq.map(fun searchStep ->
                R.com<SearchItem,_,_>
                    { searchStep = searchStep
                    ; step = 0 
                    ; onSelect = fun _ -> this.select(searchStep)} [])
            |> Seq.toList
        R.div [ClassName "row"] [
            R.div [ClassName "col-md-6 col-md-offset-3"] [
                R.h1 [] [R.str "Vehicle Search"]
                R.div [ClassName "list-group"] brandItems 
            ]
        ] |> Some

// Firing up the app
let model = SearchModel("react-veh-search")
let render() =
    ReactDom.render(
        R.com<VehicleSearchApp,_,_> { model = model } [],
        Browser.document.getElementsByClassName("searchapp").[0]
    )
model.subscribe(render)
render()