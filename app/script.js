import "../templates/css/reset.css";
import "./style.css";
import { Elm } from "./Main.elm";

var mountNode = document.getElementById("elmApp");

Elm.Main.init({ node: mountNode, flags: {} });
