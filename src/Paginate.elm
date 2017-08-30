module Paginate
    exposing
        ( Paginated
        , initial
          -- Config
        , Config
        , FetchResponse
        , makeConfig
          -- Retrieving Data
        , getCurrent
        , getPage
        , getTotalPages
        , getTotalItems
        , getError
        , getData
          -- Querying
        , isLoading
        , hasNone
        , hasPrevious
        , hasNext
          -- Modification
        , moveNext
        , movePrevious
        , jumpTo
        , updateData
          -- Update / Messages
        , Msg
        , update
        )

{-| `Paginate` is used for querying & paginating API responses. Paginate will
handle fetching new pages of data, and caches already fetched data.

TODO: Eventually:

  - examples
  - fetchRequest per-args cache?
      - toString on args to get a (Dict String (Dict PageNumber Chunk))?
  - custom page sizes(+ reorganize items when changed)

# Model

@docs Paginated, initial

# Config

@docs Config, FetchResponse, makeConfig

# Retrieving Data

@docs getCurrent, getPage, getTotalPages, getTotalItems, getError, getData

# Querying Status

@docs isLoading, hasNone, hasPrevious, hasNext

# Modifying Pagination

@docs moveNext, movePrevious, jumpTo, updateData

# Updating / Messages

@docs Msg, update

-}

import Dict exposing (Dict)
import Http
import RemoteData exposing (WebData)


-- Model


{-| A Chunk is a list of items annotated with a page number.
-}
type Chunk a
    = Chunk { items : List a, page : Int }


{-| The `Paginated` type is responsible for storing the fetched items, current
page number, total count, and additional data to pass to the fetch command..
-}
type Paginated a b
    = Paginated
        { items : Dict Int (WebData (Chunk a))
        , currentPage : Int
        , totalCount : Int
        , requestData : b
        }


{-| The result type of a Paginated Fetch Request. At the minimum, your API
needs to return the items & a total count of all items.
-}
type alias FetchResponse a =
    { items : List a
    , totalCount : Int
    }


{-| The `Config` type is used to build a Fetch Request, given the Paginated's
Request Data & a Page Number.
-}
type Config a b
    = Config
        { fetchRequest : b -> Int -> Http.Request (FetchResponse a)
        }


{-| Make a `Config` from a function that takes a list of Parameters & a Page Number.
-}
makeConfig : (b -> Int -> Http.Request (FetchResponse a)) -> Config a b
makeConfig fetchRequest =
    Config { fetchRequest = fetchRequest }


{-| Get an initial Pagination & Fetch Commands from a `Config`, list of Filters,
& Page Number.
-}
initial : Config a b -> b -> Int -> ( Paginated a b, Cmd (Msg a) )
initial config requestData page =
    let
        initialModel =
            Paginated
                { items = Dict.empty
                , currentPage = page
                , totalCount = 0
                , requestData = requestData
                }
    in
        ( initialModel
        , getFetches config initialModel
        )


{-| Get the current list of items.
-}
getCurrent : Paginated a b -> List a
getCurrent (Paginated { items, currentPage }) =
    Dict.get currentPage items
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map (\(Chunk { items }) -> items)
        |> Maybe.withDefault []


{-| Get the current page number.
-}
getPage : Paginated a b -> Int
getPage (Paginated { currentPage }) =
    currentPage


{-| Get the total number of pages.
-}
getTotalPages : Paginated a b -> Int
getTotalPages (Paginated { totalCount }) =
    ceiling <| toFloat totalCount / toFloat 25


{-| Get the total item count.
-}
getTotalItems : Paginated a b -> Int
getTotalItems (Paginated { totalCount }) =
    totalCount


{-| Return the current page's fetch request's error if it has one.
-}
getError : Paginated a b -> Maybe Http.Error
getError (Paginated { items, currentPage }) =
    case Dict.get currentPage items of
        Just (RemoteData.Failure e) ->
            Just e

        _ ->
            Nothing


{-| Return the Extra Request Data for the current Paginated.
-}
getData : Paginated a b -> b
getData (Paginated { requestData }) =
    requestData


{-| Did the current page load successfully but return no items?
-}
hasNone : Paginated a b -> Bool
hasNone (Paginated { items, currentPage }) =
    case Dict.get currentPage items of
        Just (RemoteData.Success chunk) ->
            getChunkItems chunk
                |> List.isEmpty

        _ ->
            False


{-| Is the current page's fetch request still loading?
-}
isLoading : Paginated a b -> Bool
isLoading (Paginated { items, currentPage }) =
    case Dict.get currentPage items of
        Just (RemoteData.Loading) ->
            True

        Just _ ->
            False

        _ ->
            True


{-| Are there page's before the current one?
-}
hasPrevious : Paginated a b -> Bool
hasPrevious (Paginated { currentPage }) =
    currentPage /= 1


{-| Are there page's after the current one?
-}
hasNext : Paginated a b -> Bool
hasNext ((Paginated { currentPage }) as pagination) =
    currentPage /= getTotalPages pagination


{-| Move to the next page.
TODO: re-implement as call to `jumpTo`?
-}
moveNext : Config a b -> Paginated a b -> ( Paginated a b, Cmd (Msg a) )
moveNext (Config config) ((Paginated pagination) as model) =
    let
        currentPage =
            getPage (Paginated pagination)

        updatedModel =
            Dict.get (currentPage + 1) pagination.items
                |> Maybe.map
                    (\_ -> Paginated { pagination | currentPage = currentPage + 1 })
                |> Maybe.withDefault model
    in
        ( updatedModel, getFetches (Config config) updatedModel )


{-| Move to the previous page.
TODO: re-implement as call to `jumpTo`?
-}
movePrevious : Config a b -> Paginated a b -> ( Paginated a b, Cmd (Msg a) )
movePrevious (Config config) ((Paginated pagination) as model) =
    let
        currentPage =
            getPage (Paginated pagination)

        updatedModel =
            Dict.get (currentPage - 1) pagination.items
                |> Maybe.map
                    (\_ -> Paginated { pagination | currentPage = currentPage - 1 })
                |> Maybe.withDefault model
    in
        ( updatedModel, getFetches (Config config) updatedModel )


{-| Move to a specific page.
-}
jumpTo : Config a b -> Paginated a b -> Int -> ( Paginated a b, Cmd (Msg a) )
jumpTo (Config config) ((Paginated pagination) as model) page =
    let
        canJump =
            page > 0 && page <= getTotalPages (Paginated pagination)

        jumpDifference =
            page - getPage (Paginated pagination)

        updatedModel =
            if canJump then
                Paginated { pagination | currentPage = page }
            else
                model
    in
        ( updatedModel, getFetches (Config config) updatedModel )


{-| Replace the current Extra Request Data, jumping to page 1 & performing new
fetch requests. Does nothing if the Data is equal to existing Data.
-}
updateData : Config a b -> Paginated a b -> b -> ( Paginated a b, Cmd (Msg a) )
updateData config ((Paginated pagination) as model) newData =
    if newData == pagination.requestData then
        ( model, Cmd.none )
    else
        initial config newData 1



-- Update


{-| Wrap an Http Response for a Page, to be handled in the `update` function.
-}
type Msg a
    = FetchPage Int (WebData (FetchResponse a))


{-| Update the Paginated Model on Fetch Completion.

If the Page Number of the Returned Model is Different than the Given Model,
We Were On a Page That Didn't Exist, So We Changed to the Last Page.

You Should Handle this Special Case in your Update Function, so that You can
Modify the Page URL.

-}
update : Config a b -> Msg a -> Paginated a b -> ( Paginated a b, Cmd (Msg a) )
update config msg (Paginated model) =
    case msg of
        FetchPage page ((RemoteData.Failure e) as data) ->
            let
                _ =
                    Debug.log "Fetch Error: "
                        data
            in
                ( Paginated
                    { model
                        | items = Dict.insert page (RemoteData.Failure e) model.items
                    }
                , Cmd.none
                )

        FetchPage page (RemoteData.Success { items, totalCount }) ->
            let
                newChunk =
                    Chunk { items = items, page = page }

                updatedModel =
                    Paginated
                        { model
                            | totalCount = totalCount
                            , items = Dict.insert page (RemoteData.succeed newChunk) model.items
                        }

                pageIsEmptyButPagesExist =
                    List.isEmpty items && getTotalPages updatedModel > 0
            in
                if pageIsEmptyButPagesExist && page == model.currentPage then
                    jumpTo config updatedModel (getTotalPages updatedModel)
                else
                    ( updatedModel, Cmd.none )

        FetchPage page data ->
            let
                newData =
                    RemoteData.map (\{ items } -> Chunk { items = items, page = page })
                        data
            in
                ( Paginated { model | items = Dict.insert page newData model.items }
                , Cmd.none
                )



-- Utils


{-| Get the items for a `Chunk`
-}
getChunkItems : Chunk a -> List a
getChunkItems (Chunk { items }) =
    items


{-| Return the Fetch commands for the current page. This will also prefetch the
previous/next pages if they exist.
-}
getFetches : Config a b -> Paginated a b -> Cmd (Msg a)
getFetches (Config config) (Paginated pagination) =
    let
        currentPage =
            getPage (Paginated pagination)

        totalPages =
            getTotalPages (Paginated pagination)

        hasItems offset =
            Dict.get (pagination.currentPage + offset) pagination.items
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map (not << List.isEmpty << getChunkItems)
                |> Maybe.withDefault False

        currentFetch =
            if not <| hasItems 0 then
                config.fetchRequest pagination.requestData currentPage
                    |> RemoteData.sendRequest
                    |> Cmd.map (FetchPage currentPage)
            else
                Cmd.none

        previousFetch =
            if not <| hasItems -1 then
                config.fetchRequest pagination.requestData (currentPage - 1)
                    |> RemoteData.sendRequest
                    |> Cmd.map (FetchPage <| currentPage - 1)
            else
                Cmd.none

        nextFetch =
            if not <| hasItems 1 then
                config.fetchRequest pagination.requestData (currentPage + 1)
                    |> RemoteData.sendRequest
                    |> Cmd.map (FetchPage <| currentPage + 1)
            else
                Cmd.none
    in
        if currentPage > 1 && (currentPage < totalPages || totalPages == 0) then
            Cmd.batch [ currentFetch, previousFetch, nextFetch ]
        else if currentPage > 1 then
            Cmd.batch [ currentFetch, previousFetch ]
        else if (currentPage < totalPages || totalPages == 0) then
            Cmd.batch [ currentFetch, nextFetch ]
        else
            currentFetch
