module ReactVehSearch

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Microsoft.FSharp.Reflection

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

// this simulates the database
type VehiculeItemDataRecord = { id: int; label: string; parentId: int; category: string }
let datas = [ { id = 1; label = "Renault"; parentId = 0; category = "Make" }
            ; { id = 2; label = "Volkswagen"; parentId = 0; category = "Make" }
            ; { id = 3; label = "Mercedes"; parentId = 0; category = "Make" }
            ; { id = 4; label = "BMW"; parentId = 0; category = "Make" }
            ; { id = 5; label = "Opel"; parentId = 0; category = "Make" }
            ; { id = 6; label = "Clio"; parentId = 1; category = "Model" }
            ; { id = 7; label = "Megane"; parentId = 1; category = "Model" }
            ; { id = 8; label = "Talisman"; parentId = 1; category = "Model" }
            ; { id = 9; label = "Passat"; parentId = 2; category = "Model" }
            ; { id = 10; label = "Tuareg"; parentId = 2; category = "Model" }
            ; { id = 11; label = "C"; parentId = 3; category = "Model" }
            ; { id = 12; label = "X6"; parentId = 4; category = "Model" }
            ; { id = 13; label = "Série 5"; parentId = 4; category = "Model" }
            ; { id = 14; label = "Corsa"; parentId = 5; category = "Model" }
            ; { id = 15; label = "Insignia"; parentId = 5; category = "Model" }
            ; { id = 16; label = "Essence"; parentId = 9; category = "Fuel" }
            ; { id = 17; label = "Diesel"; parentId = 9; category = "Fuel" }
            ; { id = 18; label = "Manuelle"; parentId = 16; category = "Transmission" }
            ; { id = 19; label = "Automatique"; parentId = 16; category = "Transmission" }
            ; { id = 20; label = "Sémi-automatique"; parentId = 16; category = "Transmission" }
            ; { id = 21; label = "Manuelle"; parentId = 17; category = "Transmission" }
            ; { id = 22; label = "Automatique"; parentId = 17; category = "Transmission" }
            ; { id = 23; label = "Berline"; parentId = 22; category = "Body" }
            ; { id = 24; label = "Break"; parentId = 22; category = "Body" }
            ; { id = 25; label = "14"; parentId = 24; category = "FiscalPower" }
            ; { id = 26; label = "7"; parentId = 24; category = "FiscalPower" }
            ]

type Table = { col1: string; col2: string; col3: string; col4: string }
let results = [ { col1 = "14"; col2 = "Passat Confort Line"; col3 = "VW0003423"; col4 = "Volkswagen Passat SW"}
              ; { col1 = "14"; col2 = "Passat Carat"; col3 = "VW110023"; col4 = "Volkswagen Passat SW"}]

// Model
type VehicleItem = { id: int; label: string }

type SearchStep =
| Make
| Model
| Fuel
| Transmission
| Body
| FiscalPower
| Result

type Search = { id: Guid; lastChoice: SearchStep option; selectedChoices: VehicleItem list; showResults: bool }

let getNextSearchStep currentSearchStep =
    match currentSearchStep with
    | Make -> Model
    | Model -> Fuel
    | Fuel -> Transmission
    | Transmission -> Body
    | Body -> FiscalPower
    | FiscalPower -> Result
    | Result -> Result

let getSearchStepName currentSearchStep =
    match currentSearchStep with
    | Make -> "Make"
    | Model -> "Model"
    | Fuel -> "Fuel"
    | Transmission -> "Transmission"
    | Body -> "Body"
    | FiscalPower -> "Fiscal Power"
    | Result -> "Vehicle list"

[<PassGenericsAttribute>]
let getData category selectedId =
    let toString (x:'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name
    datas
    |> List.filter (fun e -> e.category = toString category && e.parentId = selectedId)
    |> List.map (fun e -> { id = e.id; label = e.label})
    |> List.sortBy (fun e -> e.label)

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
        this.search <- { this.search with selectedChoices =  selectedItem :: this.search.selectedChoices }
        this.inform()


module R = Fable.Helpers.React
open R.Props

// result component
type [<Pojo>] SearchResultProps =
    { resultList: Table list }

let SearchResult(props: SearchResultProps) =
    let rows = 
        props.resultList
            |> List.map (fun row -> 
                R.tr [] [
                    R.td [] [R.str row.col1]
                    R.td [] [R.str row.col2]
                    R.td [] [R.str row.col3]
                    R.td [] [R.str row.col4]
                ])
    R.table [ClassName "table table-hover"] [
        R.thead [] [
            R.tr [] [
                R.th [] [R.str "Fiscal Power"]
                R.th [] [R.str "Model Name"]
                R.th [] [R.str "Type"]
                R.th [] [R.str "Description"]
            ]
        ]
        R.tbody [] rows
    ]

type [<Pojo>] SearchItemProps =
    { selectionItem: VehicleItem
    ; onSelect: React.SyntheticEvent->unit }

type [<Pojo>] SearchItemState =
    { isSelected: bool }

type SearchItem(props) =
    inherit React.Component<SearchItemProps, SearchItemState>(props)
    do base.setInitState({ isSelected = false })

    member this.render () =
        R.a [
            ClassName "list-group-item"
            !!("data-id", this.props.selectionItem.id |> string)
            OnClick (fun e -> this.props.onSelect(upcast e)) ] [R.str this.props.selectionItem.label]

// component
type [<Pojo>] SearchItemListProps =
    { searchChoices: VehicleItem list
    ; onSelect: VehicleItem->unit }

let SearchItemList(props: SearchItemListProps) =
    let brandItems =
        props.searchChoices
        |> Seq.map(fun choice ->
            R.com<SearchItem,_,_>
                { selectionItem = choice
                ; onSelect = fun _ -> props.onSelect(choice) } [])
        |> Seq.toList
    R.div [ClassName "list-group"] brandItems 

// component
type [<Pojo>] SearchItemListContainerProps =
    { searchStep: SearchStep
    ; selectedId: int 
    ; onSelect: VehicleItem->unit }

type [<Pojo>] SearchItemListContainerState =
    { searchChoices: VehicleItem list
    ; resultList: Table list }

type SearchItemListContainer(props) =
    inherit React.Component<SearchItemListContainerProps, SearchItemListContainerState>(props)
    do base.setInitState({ searchChoices = []; resultList = [] })

    member this.componentDidMount () =
        // this could be an ajax call (this is called only once)
        let searchChoices = getData this.props.searchStep this.props.selectedId
        this.setState({this.state with searchChoices = searchChoices})

    member this.componentWillReceiveProps(nextProps) =
        // this could be an ajax call
        match nextProps.searchStep with
        | Result -> 
            this.setState({this.state with resultList = results})
        | _ -> 
            let searchChoices = getData nextProps.searchStep nextProps.selectedId
            this.setState({this.state with searchChoices = searchChoices})

    member this.render () =
        match this.props.searchStep with
        | Result -> 
            R.fn SearchResult { resultList = this.state.resultList } []
        | _ -> R.fn SearchItemList { searchChoices = this.state.searchChoices; onSelect = this.props.onSelect } []

// Vehicle Search view app
type [<Pojo>] VehicleSearchAppProps =
    { model: SearchModel }

type [<Pojo>] VehicleSearchAppState =
    { currentSearchStep: SearchStep
    ; nextSearchStep: SearchStep
    ; selectedId: int
    ; searching: Guid option }

type VehicleSearchApp(props) =
    inherit React.Component<VehicleSearchAppProps, VehicleSearchAppState>(props)
    do base.setInitState({ currentSearchStep = Make; nextSearchStep = getNextSearchStep Make; selectedId = 0; searching = System.Guid.NewGuid() |> Some })

    member this.select (selectedItem) =
        this.props.model.select(selectedItem)
        this.setState({ this.state with currentSearchStep = this.state.nextSearchStep; nextSearchStep = getNextSearchStep this.state.nextSearchStep; selectedId = selectedItem.id })

    member this.render () =
        
        R.div [] [
            R.div [ClassName "row"] [
                R.div [ClassName "col-md-4 col-md-offset-4"] [
                    R.h1 [] [R.str "Vehicle Search"]
                    R.h4 [] [R.str ("Select " + getSearchStepName this.state.currentSearchStep)]
                ]
            ]
            R.div [ClassName "row"] [
                R.div [ClassName "col-md-2 col-md-offset-1"] [
                    R.div [ClassName "row"] [
                        R.div [ClassName "alert alert-info alert-dismissible"] [
                            R.button [
                                ClassName "close" 
                                !!("data-dismiss", "alert")
                                !!("aria-label", "close")] [
                                    R.span [!!("aria-hidden", "true")] [R.str "×"]
                                ]
                            R.str "Volkswagen"
                        ]
                    ]
                ]
                R.div [ClassName "col-md-1"][]
                R.div [ClassName "col-md-4"] [
                    R.div [ClassName "row"] [
                        R.div [] [
                            R.com<SearchItemListContainer,_,_> 
                                { selectedId = this.state.selectedId
                                ; searchStep = this.state.currentSearchStep 
                                ; onSelect = this.select } []
                        ]
                    ]
                ]
            ]
        ]

// Firing up the app
let model = SearchModel("react-veh-search")
let render() =
    ReactDom.render(
        R.com<VehicleSearchApp,_,_> { model = model } [],
        Browser.document.getElementsByClassName("searchapp").[0]
    )
model.subscribe(render)
render()