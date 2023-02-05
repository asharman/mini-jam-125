import "./style.css";
import { Elm } from "./src/Main.elm";
import * as Tone from "tone";

window.addEventListener("click", () => {
  Tone.start();
});

const root = document.querySelector("#app");
const app = Elm.Main.init({
  node: root,
  flags: [window.innerWidth, window.innerHeight],
});

app.ports.audioMsg.subscribe(({ message, tempo, spawns }) => {
  switch (message) {
    case "gameStarted":
      newGame();
      break;

    case "playerJumped":
      break;

    case "obstacleSpawned":
      obstacleNote(spawns - 1, tempo)
      break;

    case "tempoIncrease":
      break;

    case "tempoDecrease":
      break;


    case "gameOver":
      gameOver();
      break;

    default:
      console.log(`Unknown Message: ${message}`);
      break;
  }
});

//setup instruments
const panner = new Tone.Panner(0).toDestination();
const synth = initializeGameSynth().connect(panner)

const bass = [
  "B2", "F#3", "B3",
  "F3", "C4", "F4",
  "D3", "A3", "D4",
  "E3", "A3", "E4"]

function obstacleNote(spawns, tempo) {
  synth.triggerAttackRelease(
    bass[spawns % 12],
    0.3,
    Tone.now(),
    Math.min(0.2 * tempo, 0.5));
}

function initializeGameSynth() {
  var instrument = new Tone.FMSynth();
  var synthJSON = {
    "harmonicity": 5,
    "modulationIndex": 10,
    "oscillator": {
      "type": "sine"
    },
    "envelope": {
      "attack": 0.001,
      "decay": 2,
      "sustain": 0.1,
      "release": 2
    },
    "modulation": {
      "type": "square"
    },
    "modulationEnvelope": {
      "attack": 0.002,
      "decay": 0.2,
      "sustain": 0,
      "release": 0.2
    }
  }

  instrument.set(synthJSON);

  instrument.connect(Tone.Destination);


  return instrument
}

function newGame() {
  synth.triggerAttackRelease("A3", "8n", undefined, 0.2);
}

function gameOver() {
  synth.triggerAttackRelease("C#2", "16n", Tone.now(), 0.3);
}