module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Tuple
import Random


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Jugador =
    { nombre : String
    , puntos : Int
    }


type alias Model =
    { jugadores : List (Jugador), jugador : String, patente : Maybe Patente }


type alias Letra =
    { letra : Char, valor : Int, cantidad : Int }


type Patente
    = Patente Letra Letra Letra


init : () -> ( Model, Cmd Msg )
init _ =
    ( { jugadores = [], jugador = "", patente = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = NombreJugador String
    | NuevoJugador
    | ChauJugador Jugador
    | Empezar
    | NuevaPatente Patente


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NombreJugador j ->
            ( { model | jugador = j }, Cmd.none )

        NuevoJugador ->
            ( { model | jugadores = model.jugadores ++ [ { nombre = model.jugador, puntos = 0 } ], jugador = "" }, Cmd.none )

        ChauJugador nombre ->
            let
                nuevos =
                    List.filter (\e -> e /= nombre) model.jugadores
            in
                ( { model | jugadores = nuevos }, Cmd.none )

        Empezar ->
            ( model, Random.generate NuevaPatente rndPatente )

        NuevaPatente nuevaPatente ->
            ( { model | patente = Just nuevaPatente }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.patente of
        Nothing ->
            elegirJugadores model

        Just p ->
            juego p


juego : Patente -> Html Msg
juego patente =
    text (show patente)


elegirJugadores : Model -> Html Msg
elegirJugadores model =
    div []
        [ div []
            [ input [ type_ "text", placeholder "Nombre", onInput NombreJugador, value model.jugador ] []
            , button [ onClick NuevoJugador, disabled (model.jugador |> String.trim |> String.isEmpty) ] [ text "Agregar" ]
            , button [ onClick Empezar, disabled (List.isEmpty model.jugadores) ] [ text "Empezar" ]
            ]
        , div []
            [ listaJugadores model.jugadores ]
        ]


listaJugadores : List Jugador -> Html Msg
listaJugadores lista =
    if List.isEmpty lista then
        text "No juega nadie :("
    else
        div []
            [ text "Juegan: "
            , lista
                |> List.map jugadorLi
                |> ul [ class "list-group" ]
            ]


jugadorLi : Jugador -> Html Msg
jugadorLi jugador =
    li []
        [ text jugador.nombre
        , button
            [ onClick (ChauJugador jugador) ]
            [ text "X" ]
        ]



-- UTILS


show : Patente -> String
show (Patente j k l) =
    String.fromList
        [ j.letra
        , k.letra
        , l.letra
        ]


cycle : List a -> List a
cycle l =
    case l of
        [] ->
            []

        x :: xs ->
            xs ++ [ x ]


letras : List (Letra)
letras =
    [ { letra = 'A', valor = 1, cantidad = 12 }
    , { letra = 'E', valor = 1, cantidad = 12 }
    , { letra = 'O', valor = 1, cantidad = 9 }
    , { letra = 'I', valor = 1, cantidad = 6 }
    , { letra = 'S', valor = 1, cantidad = 6 }
    , { letra = 'N', valor = 1, cantidad = 5 }
    , { letra = 'R', valor = 1, cantidad = 5 }
    , { letra = 'U', valor = 1, cantidad = 5 }
    , { letra = 'L', valor = 1, cantidad = 4 }
    , { letra = 'T', valor = 1, cantidad = 4 }
    , { letra = 'D', valor = 2, cantidad = 5 }
    , { letra = 'G', valor = 2, cantidad = 2 }
    , { letra = 'C', valor = 3, cantidad = 4 }
    , { letra = 'B', valor = 3, cantidad = 2 }
    , { letra = 'M', valor = 3, cantidad = 2 }
    , { letra = 'P', valor = 3, cantidad = 2 }
    , { letra = 'H', valor = 4, cantidad = 2 }
    , { letra = 'F', valor = 4, cantidad = 1 }
    , { letra = 'V', valor = 4, cantidad = 1 }
    , { letra = 'Y', valor = 4, cantidad = 1 }
    , { letra = 'Q', valor = 5, cantidad = 1 }
    , { letra = 'J', valor = 8, cantidad = 1 }
    , { letra = 'Ñ', valor = 8, cantidad = 1 }
    , { letra = 'X', valor = 8, cantidad = 1 }
    , { letra = 'Z', valor = 10, cantidad = 1 }
    ]


rndPatente : Random.Generator Patente
rndPatente =
    Random.map3 Patente rndLetra rndLetra rndLetra


rndLetra : Random.Generator Letra
rndLetra =
    case weightedLetras of
        x :: xs ->
            Random.weighted x xs

        _ ->
            Debug.todo "al horno con papas"


weightedLetras : List ( Float, Letra )
weightedLetras =
    List.map (\l -> ( toFloat l.cantidad, l )) letras
