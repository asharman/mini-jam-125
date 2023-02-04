import "./style.css";
import { Elm } from "./src/Main.elm";
import * as Tone from "tone";

//create a synth and connect it to the main output (your speakers)
const synth = new Tone.Synth().toDestination();
window.addEventListener("click", () => {
  Tone.start();
});

const root = document.querySelector("#app");
const app = Elm.Main.init({
  node: root,
  flags: [window.innerWidth, window.innerHeight],
});

app.ports.audioMsg.subscribe(({ message, tempo }) => {
  console.log(tempo);
  switch (message) {
    case "gameStarted":
      synth.triggerAttackRelease("C4", "8n");
      break;

    case "gameOver":
      synth.triggerAttackRelease("C2", "8n");
      break;

    default:
      console.log(`Unknown Message: ${message}`);
      break;
  }
});
