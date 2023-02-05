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
      nextNote(spawns - 1, tempo)
      console.log("ObstacleSpawned", spawns - 1)
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

const feedbackDelay = new Tone.FeedbackDelay("4n", 0.3);
const autoPanner = new Tone.AutoPanner("4n").start();
const bassVoice = initializeGameSynth()
const melodyVoice = initializeGameSynth();




const bass = [
  "B2", "F#3", "B3",
  "F3", "C4", "F4",
  "B2", "F#3", "B3",
  "G3", "D4", "G4",
  "B2", "F#3", "B3",
  "F3", "C4", "F4",
  "D3", "A3", "D4",
  "E3", "A3", "E4"]


const melody = [
  "B5", "C#5", "D5",
  "C5", "C5", "C5",
  "B5", "C#5", "D5",
  "E5", "E5", "E5",
  "B5", "C#5", "D5",
  "C5", "A5", "A5",
  "D5", "E5", "F#5",
  "E5", "D5", "C#5"
]

function bassNote(spawns, tempo) {
  bassVoice.triggerAttack(
    bass[spawns % 24],
    Tone.now(),
    Math.min(0.2 * tempo, 0.5));
}

function melodyNote(spawns, tempo) {
  melodyVoice.triggerAttack(
    melody[spawns % 24],
    Tone.now(),
    Math.min(0.3 * tempo, 0.5));
}



function nextNote(spawns, tempo) {
  bassNote(spawns, tempo)
  if (spawns > 23) {
    melodyNote(spawns, tempo)
  }
}

function initializeGameSynth() {
  const instrument = new Tone.FMSynth();
  const synthJSON = {
    "harmonicity": 5,
    "modulationIndex": 10,
    "oscillator": {
      "type": "sine"
    },
    "envelope": {
      "attack": 0.001,
      "decay": 2,
      "sustain": 0,
      "release": 1
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
  instrument.chain(
    feedbackDelay,
    autoPanner,
    Tone.Destination);

  return instrument
}

function newGame() {
  bassVoice.triggerAttack("A3", Tone.now(), 0.2);
}

function gameOver() {
  bassVoice.triggerAttack("C#2", Tone.now(), 0.3);
}