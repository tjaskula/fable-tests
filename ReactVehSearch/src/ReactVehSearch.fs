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
type Brand = { Id: int; Label: string }
type Make = { id: int; brandId: int; label: string }

type SearchStepItem = { id: int; label: string }
type Search = { id: Guid; searchSteps: SearchStepItem list; currentStep: int }

type SearchModel(key) =
    member val key = key
    member val search: Search = defaultArg (Util.load key) {id = System.Guid.NewGuid(); searchSteps = []; currentStep = 0} with get, set
    member val onChanges: (unit->unit)[] = [||] with get, set

    member this.subscribe(onChange) =
        this.onChanges <- [|onChange|]


module R = Fable.Helpers.React
open R.Props

type [<Pojo>] SearchItemProps =
    { id: int
    ; label: string
    ; step: int
    ; onSelect: string->unit
    ; onCancel: React.SyntheticEvent->unit }

type [<Pojo>] SearchItemState =
    { currentStep: int }

type SearchItem(props) =
    inherit React.Component<SearchItemProps, SearchItemState>(props)
    do base.setInitState({ currentStep = props.step })

    member this.render () =
        R.a [
            ClassName "list-group-item"
            !!("data-id", (this.props.id |> string)) ] [R.str this.props.label]


// Vehicle Search view app
type [<Pojo>] VehicleSearchAppProps =
    { model: SearchModel }

type [<Pojo>] VehicleSearchAppState =
    { currentStep: int
    ; searching: Guid option
    ; brands: Brand list }

type VehicleSearchApp(props) =
    inherit React.Component<VehicleSearchAppProps, VehicleSearchAppState>(props)
    do base.setInitState({ currentStep = 0; searching = None; brands = [] })

    member this.componentDidMount () =
        let brands = [{ Id = 1; Label = "Mercedes"}; { Id = 2; Label = "BMW" }]
        this.setState({this.state with brands = brands})
        
    member this.cancel () =
        this.setState({ this.state with currentStep = 0 })

    member this.render () =
        let brands = this.state.brands
        let brandItems =
            brands
            |> Seq.map(fun s ->
                R.com<SearchItem,_,_>
                    { id = s.Id
                    ; label = s.Label
                    ; step = 0
                    ; onSelect = fun _ -> this.cancel()
                    ; onCancel = fun _ -> this.cancel() } [])
            |> Seq.toList
        R.div [ClassName "list-group"] brandItems |> Some

// Firing up the app
let model = SearchModel("react-veh-search")
let render() =
    ReactDom.render(
        R.com<VehicleSearchApp,_,_> { model = model } [],
        Browser.document.getElementsByClassName("searchapp").[0]
    )
model.subscribe(render)
render()