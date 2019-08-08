module Paginate exposing
    ( Paginated, initial
    , Config, FetchResponse, makeConfig
    , getCurrent, getPage, getPerPage, getTotalPages, getTotalItems, getError, getRequestData, getResponseData, getRemoteData
    , getPagerSections, bootstrapPager
    , isLoading, hasNone, isFirst, isLast, hasPrevious, hasNext
    , moveNext, movePrevious, jumpTo, updateData, updatePerPage
    , Msg, update
    )

{-| `Paginate` is used for querying & paginating API responses. Paginate will
handle fetching new pages of data, and caches already fetched data.

TODO: Eventually:

  - examples
  - fetchRequest per-args cache?
      - toString on args to get a (Dict String (Dict PageNumber Chunk))?


# Model

@docs Paginated, initial


# Config

@docs Config, FetchResponse, makeConfig


# Retrieving Data

@docs getCurrent, getPage, getPerPage, getTotalPages, getTotalItems, getError, getRequestData, getResponseData, getRemoteData


# Rendering

@docs getPagerSections, bootstrapPager


# Querying Status

@docs isLoading, hasNone, isFirst, isLast, hasPrevious, hasNext


# Modifying Pagination

@docs moveNext, movePrevious, jumpTo, updateData, updatePerPage


# Updating / Messages

@docs Msg, update

-}

import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, li, span, text)
import Html.Attributes exposing (class)
import Http
import RemoteData exposing (WebData)



-- Model


{-| A Chunk is a list of items annotated with a page number.
-}
type Chunk a
    = Chunk { items : List a, page : Int }


{-| The `Paginated` type is responsible for storing the fetched items, current
page number, total count, and additional data to pass to the fetch command..

The `a` type refers to the data that is paginated, `b` refers to any additional
data you need to make the API request, and `c` is any additional data you want
to pull out of the API response.

-}
type Paginated a b c
    = Paginated
        { items : Dict Int (WebData (Chunk a))
        , currentPage : Int
        , perPage : Int
        , totalCount : Int
        , requestData : b
        , responseData : Maybe c
        }


{-| The result type of a Paginated Fetch Request. At the minimum, your API
needs to return the items & a total count of all items.

If any additional data is returned that you would like to have access to, you
can decode it to the `extraData` field and use `getResponseData` to pull it
out of a `Paginated`.

-}
type alias FetchResponse a c =
    { items : List a
    , totalCount : Int
    , extraData : Maybe c
    }


{-| The `Config` type is used to build a Fetch Request, given the Paginated's
Request Data, a Page Number, & the Items per Page.
-}
type Config a b c
    = Config
        { fetchRequest : b -> Int -> Int -> Cmd (WebData (FetchResponse a c))
        }


{-| Make a `Config` from a function that takes a list of Parameters & a Page Number.
-}
makeConfig : (b -> Int -> Int -> Cmd (WebData (FetchResponse a c))) -> Config a b c
makeConfig fetchRequest =
    Config { fetchRequest = fetchRequest }


{-| Get an initial Pagination & Fetch Commands from a `Config`, list of Filters,
& Page Number.
-}
initial : Config a b c -> b -> Int -> Int -> ( Paginated a b c, Cmd (Msg a c) )
initial config requestData page perPage =
    let
        initialModel =
            Paginated
                { items = Dict.empty
                , currentPage = page
                , perPage = perPage
                , totalCount = 0
                , requestData = requestData
                , responseData = Nothing
                }
    in
    ( initialModel
    , getFetches config initialModel
    )


{-| Get the current list of items.
-}
getCurrent : Paginated a b c -> List a
getCurrent (Paginated { items, currentPage }) =
    Dict.get currentPage items
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map (\(Chunk c) -> c.items)
        |> Maybe.withDefault []


{-| Get the current page number.
-}
getPage : Paginated a b c -> Int
getPage (Paginated { currentPage }) =
    currentPage


{-| Get the number of items to show per page.
-}
getPerPage : Paginated a b c -> Int
getPerPage (Paginated { perPage }) =
    perPage


{-| Get the total number of pages.
-}
getTotalPages : Paginated a b c -> Int
getTotalPages (Paginated { totalCount, perPage }) =
    ceiling <| toFloat totalCount / toFloat perPage


{-| Get the total item count.
-}
getTotalItems : Paginated a b c -> Int
getTotalItems (Paginated { totalCount }) =
    totalCount


{-| Return the current page's fetch request's error if it has one.
-}
getError : Paginated a b c -> Maybe Http.Error
getError (Paginated { items, currentPage }) =
    case Dict.get currentPage items of
        Just (RemoteData.Failure e) ->
            Just e

        _ ->
            Nothing


{-| Return the Extra Request Data for the current Paginated.
-}
getRequestData : Paginated a b c -> b
getRequestData (Paginated { requestData }) =
    requestData


{-| Return any Extra Response Data for the current Paginated.
-}
getResponseData : Paginated a b c -> Maybe c
getResponseData (Paginated { responseData }) =
    responseData


{-| Return the raw `WebData` for the current page.
-}
getRemoteData : Paginated a b c -> WebData (List a)
getRemoteData (Paginated { items, currentPage }) =
    case Dict.get currentPage items of
        Nothing ->
            RemoteData.NotAsked

        Just data ->
            RemoteData.map (\(Chunk c) -> c.items) data


{-| Did the current page load successfully but return no items?
-}
hasNone : Paginated a b c -> Bool
hasNone (Paginated { items, currentPage }) =
    case Dict.get currentPage items of
        Just (RemoteData.Success chunk) ->
            getChunkItems chunk
                |> List.isEmpty

        _ ->
            False


{-| Generate sections of pages to show for a Pager. Depending on the current
page & total number of pages, the sections may either be a single section, a
beginning & end section, or a beginning, middle, & end section.

Each section's page is a tuple of `(pageNumber, isCurrentPage)`.

The sections are defined by the initial arguments, the first is the number of
pages to show at the end sections, while the second is the number of pages to
show around the current page if it is in the middle section.

Some example of Pagers generated with 2 end & middle pages, with the current
page marked by `*`:

    *1*|2|3|4|5|6

    *1*|2|3|4|5|...|49|50

    1|2|3|4|*5*|...|49|50

    1|2|...|4|5|*6*|7|8|...|49|50

    1|2|...|9|10|*11*|12|13|...|49|50

    1|2|...|*46*|47|48|49|50

No split is made if there are not enough pages to show a middle section. When
at an end section, enough pages are shown that there must be something hidden
before splitting off the middle section.

-}
getPagerSections : Int -> Int -> Paginated a b c -> List (List ( Int, Bool ))
getPagerSections endPagesToShow middlePagesToShow pagination =
    let
        currentPage =
            getPage pagination

        totalPages =
            getTotalPages pagination

        showSplit =
            totalPages >= pagesToSplitAt

        pagesToSplitAt =
            endPagesToShow * 2 + middlePagesToShow * 2 + 3

        showMiddle =
            showSplit
                && (currentPage > endPagesToShow + middlePagesToShow + 1)
                && (currentPage < totalPages - endPagesToShow - middlePagesToShow)

        pageSections =
            if showMiddle then
                [ List.range 1 endPagesToShow
                , List.range (currentPage - middlePagesToShow) (currentPage + middlePagesToShow)
                , List.range (totalPages - endPagesToShow + 1) totalPages
                ]

            else if showSplit && currentPage > endPagesToShow + middlePagesToShow + 1 then
                [ List.range 1 endPagesToShow
                , List.range (totalPages - endPagesToShow - middlePagesToShow) totalPages
                ]

            else if showSplit then
                [ List.range 1 (endPagesToShow + middlePagesToShow + 1)
                , List.range (totalPages - endPagesToShow + 1) totalPages
                ]

            else
                [ List.range 1 totalPages ]
    in
    List.map (List.map (\pageNumber -> ( pageNumber, pageNumber == currentPage )))
        pageSections


{-| Render a split Pager with the standard Bootstrap4 classes.

It takes a function that takes a page number & generates attributes for an
item's `a` element, the number of pages to show at the ends of the pagination,
the number of middle pages to show in the middles of the pagination, and a
pagination.

It will split out a list of `li` elements for each page that should be shown,
with disabled dots(`...`) between each section. A list is returned so that you
can add previous/next buttons if you want them.

See the docs for `getPagerSections` to see how the splitting works.

-}
bootstrapPager : (Int -> List (Html.Attribute msg)) -> Int -> Int -> Paginated a b c -> List (Html msg)
bootstrapPager linkAttributes endPagesToShow middlePagesToShow pagination =
    let
        itemClass isCurrent =
            if isCurrent then
                "page-item active"

            else
                "page-item"

        renderItem page isCurrent =
            li [ class <| itemClass isCurrent ]
                [ a (class "page-link" :: linkAttributes page)
                    [ text <| String.fromInt page ]
                ]

        dots =
            li [ class "page-item disabled" ]
                [ span [ class "page-link" ] [ text "..." ] ]
    in
    getPagerSections endPagesToShow middlePagesToShow pagination
        |> List.map (List.map <| \( a, b ) -> renderItem a b)
        |> List.intersperse [ dots ]
        |> List.concat


{-| Is the current page's fetch request still loading?
-}
isLoading : Paginated a b c -> Bool
isLoading (Paginated { items, currentPage }) =
    case Dict.get currentPage items of
        Just RemoteData.Loading ->
            True

        Just _ ->
            False

        _ ->
            True


{-| Is the current page the first page?
-}
isFirst : Paginated a b c -> Bool
isFirst =
    not << hasPrevious


{-| Is the current page the last page?
-}
isLast : Paginated a b c -> Bool
isLast =
    not << hasNext


{-| Are there page's before the current one?
-}
hasPrevious : Paginated a b c -> Bool
hasPrevious (Paginated { currentPage }) =
    currentPage /= 1


{-| Are there page's after the current one?
-}
hasNext : Paginated a b c -> Bool
hasNext ((Paginated { currentPage }) as pagination) =
    currentPage /= getTotalPages pagination


{-| Move to the next page.
TODO: re-implement as call to `jumpTo`?
-}
moveNext : Config a b c -> Paginated a b c -> ( Paginated a b c, Cmd (Msg a c) )
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
movePrevious : Config a b c -> Paginated a b c -> ( Paginated a b c, Cmd (Msg a c) )
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
jumpTo : Config a b c -> Int -> Paginated a b c -> ( Paginated a b c, Cmd (Msg a c) )
jumpTo (Config config) page ((Paginated pagination) as model) =
    let
        canJump =
            (page > 0 && page <= getTotalPages (Paginated pagination))
                || Dict.isEmpty pagination.items

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
updateData : Config a b c -> b -> Paginated a b c -> ( Paginated a b c, Cmd (Msg a c) )
updateData config newData ((Paginated pagination) as model) =
    if newData == pagination.requestData then
        ( model, Cmd.none )

    else
        initial config newData 1 pagination.perPage


{-| Update the items per page, jumping to page 1 & performing new fetch
requests. Does nothing if the new value is the same as the current items per
page.
-}
updatePerPage : Config a b c -> Int -> Paginated a b c -> ( Paginated a b c, Cmd (Msg a c) )
updatePerPage config newPerPage ((Paginated pagination) as model) =
    if newPerPage == pagination.perPage then
        ( model, Cmd.none )

    else
        let
            newModel =
                Paginated
                    { items = Dict.empty
                    , currentPage = 1
                    , perPage = newPerPage
                    , totalCount = 0
                    , requestData = pagination.requestData
                    , responseData = pagination.responseData
                    }
        in
        ( newModel, getFetches config newModel )



-- Update


{-| Wrap an Http Response for a Page, to be handled in the `update` function.
-}
type Msg a c
    = FetchPage Int (WebData (FetchResponse a c))


{-| Update the Paginated Model on Fetch Completion.

If the Page Number of the Returned Model is Different than the Given Model,
We Were On a Page That Didn't Exist, So We Changed to the Last Page.

You Should Handle this Special Case in your Update Function, so that You can
Modify the Page URL.

-}
update : Config a b c -> Msg a c -> Paginated a b c -> ( Paginated a b c, Cmd (Msg a c) )
update config msg (Paginated model) =
    case msg of
        FetchPage page ((RemoteData.Failure e) as data) ->
            ( Paginated
                { model
                    | items = Dict.insert page (RemoteData.Failure e) model.items
                }
            , Cmd.none
            )

        FetchPage page (RemoteData.Success { items, totalCount, extraData }) ->
            let
                newChunk =
                    Chunk { items = items, page = page }

                updatedModel =
                    Paginated
                        { model
                            | totalCount = totalCount
                            , responseData = extraData
                            , items = Dict.insert page (RemoteData.succeed newChunk) model.items
                        }

                pageIsEmptyButPagesExist =
                    List.isEmpty items && getTotalPages updatedModel > 0
            in
            if pageIsEmptyButPagesExist && page == model.currentPage then
                jumpTo config (getTotalPages updatedModel) updatedModel

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
getFetches : Config a b c -> Paginated a b c -> Cmd (Msg a c)
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
                config.fetchRequest pagination.requestData currentPage pagination.perPage
                    |> Cmd.map (FetchPage currentPage)

            else
                Cmd.none

        previousFetch =
            if not <| hasItems -1 then
                config.fetchRequest pagination.requestData (currentPage - 1) pagination.perPage
                    |> Cmd.map (FetchPage <| currentPage - 1)

            else
                Cmd.none

        nextFetch =
            if not <| hasItems 1 then
                config.fetchRequest pagination.requestData (currentPage + 1) pagination.perPage
                    |> Cmd.map (FetchPage <| currentPage + 1)

            else
                Cmd.none
    in
    if currentPage > 1 && (currentPage < totalPages || totalPages == 0) then
        Cmd.batch [ currentFetch, previousFetch, nextFetch ]

    else if currentPage > 1 then
        Cmd.batch [ currentFetch, previousFetch ]

    else if currentPage < totalPages || totalPages == 0 then
        Cmd.batch [ currentFetch, nextFetch ]

    else
        currentFetch
