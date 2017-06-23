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
type VehicleItem = { id: int; label: string }
type SearchStep =
| Make
| Model
| Fuel
| Transmissio
| Body
| FiscalPower

type Search = { id: Guid; lastChoice: SearchStep option; selectedChoices: VehicleItem list; showResults: bool }

type SearchModel(key) =
    member val key = key
    member val search: Search = defaultArg (Util.load key) {id = System.Guid.NewGuid(); lastChoice = None; selectedChoices = []; showResults = false } with get, set
    member val onChanges: (unit->unit)[] = [||] with get, set

    member this.subscribe(onChange) =
        this.onChanges <- [|onChange|]

    member this.inform() =
        Util.save this.key this.search
        this.onChanges |> Seq.iter (fun cb -> cb())

    member this.select(selectedItem) =
        Browser.console.log(selectedItem)
        this.search <- { this.search with selectedChoices =  selectedItem :: this.search.selectedChoices }
        this.inform()


module R = Fable.Helpers.React
open R.Props

type [<Pojo>] SearchItemProps =
    { selectionItem: VehicleItem
    ; onSelect: React.SyntheticEvent->unit }

type [<Pojo>] SearchItemState =
    { isSelected: bool }

type SearchItem(props) =
    inherit React.Component<SearchItemProps, SearchItemState>(props)
    do base.setInitState({ isSelected = false })

    member this.render () =
        Browser.console.log("In Render of SearchItem")
        R.a [
            ClassName "list-group-item"
            !!("data-id", this.props.selectionItem.id |> string)
            OnClick (fun e -> this.props.onSelect(upcast e)) ] [R.str this.props.selectionItem.label]

// component

type [<Pojo>] SearchItemListProps =
    { searchChoices: VehicleItem list 
    ; select: VehicleItem->unit }

type [<Pojo>] SearchItemListState =
    { isSelected: bool }

type SearchItemList(props) =
    inherit React.Component<SearchItemListProps, SearchItemListState>(props)
    do base.setInitState({ isSelected = false })

    member this.render () =
        Browser.console.log("In Render of SearchItemList")
        let searchChoices = this.props.searchChoices
        let brandItems =
            searchChoices
            |> Seq.map(fun choice ->
                R.com<SearchItem,_,_>
                    { selectionItem = choice
                    ; onSelect = fun _ -> this.props.select(choice) } [])
            |> Seq.toList
        R.div [ClassName "row"] [
            R.div [ClassName "col-md-6 col-md-offset-3"] [
                R.h1 [] [R.str "Vehicle Search"]
                R.h2 [] [R.str "Select Brand"]
                R.div [ClassName "list-group"] brandItems 
            ]
        ] |> Some

// component
type [<Pojo>] SearchItemListContainerProps =
    { searchStep: SearchStep
    ; selectedId: int option 
    ; select: VehicleItem->unit }

type [<Pojo>] SearchItemListContainerState =
    { searching: Guid option
    ; searchChoices: VehicleItem list }

type SearchItemListContainer(props) =
    inherit React.Component<SearchItemListContainerProps, SearchItemListContainerState>(props)
    do base.setInitState({ searching = System.Guid.NewGuid() |> Some; searchChoices = [] })

    member this.componentDidMount () =
        Browser.console.log("In componentDidMount of SearchItemListContainer")
        // this could be an ajax call (this is called only once)
        let searchChoices = [{ id = 1; label = "Mercedes"}; { id = 2; label = "BMW" }]
        this.setState({this.state with searchChoices = searchChoices})

    member this.render () =
        Browser.console.log("In Render of SearchItemListContainer")
        R.com<SearchItemList,_,_> { searchChoices = this.state.searchChoices; select = this.props.select } []

// Vehicle Search view app
type [<Pojo>] VehicleSearchAppProps =
    { model: SearchModel }

type [<Pojo>] VehicleSearchAppState =
    { currentStep: SearchStep
    ; selectedId: int option
    ; searching: Guid option }

type VehicleSearchApp(props) =
    inherit React.Component<VehicleSearchAppProps, VehicleSearchAppState>(props)
    do base.setInitState({ currentStep = Make; selectedId = None; searching = System.Guid.NewGuid() |> Some })

    member this.select (searchStep) =
        this.props.model.select(searchStep)

    member this.render () =
        Browser.console.log("In Render of App")
        R.com<SearchItemListContainer,_,_> 
            { selectedId = this.state.selectedId
            ; searchStep = this.state.currentStep 
            ; select = this.select } []

// Firing up the app
let model = SearchModel("react-veh-search")
let render() =
    ReactDom.render(
        R.com<VehicleSearchApp,_,_> { model = model } [],
        Browser.document.getElementsByClassName("searchapp").[0]
    )
model.subscribe(render)
render()