import "./style.css";
import { Elm } from "./src/Main.elm";
import * as Tone from "tone";

//setup instruments
const systemSynth = new Tone.Synth().toDestination();

const playerPanner = new Tone.Panner(-0.75).toDestination();
const playerSynth = new Tone.Synth().connect(playerPanner)


const enemyPanner = new Tone.Panner(0.75).toDestination();
const enemySynth = new Tone.Synth().connect(enemyPanner)

window.addEventListener("click", () => {
  Tone.start();
});

const root = document.querySelector("#app");
const app = Elm.Main.init({
  node: root,
  flags: [window.innerWidth, window.innerHeight],
});


function nextNote(spawns) {
  const notes = [
    "G", "G", "G", "G",
    "D", "D", "D", "D",
    "A", "A", "A", "A",
    "C", "C", "C", "C"]

  if (spawns < 0) {
    return "G"
  } else {
    return notes[spawns % 16]
  }
}

function setOctave(note, octave) {
  switch (note) {
    case "C":
      return "C" + `${octave + 1}`
    case "D":
      return "D" + `${octave + 1}`
    case note:
      return note + `${octave}`
  }
}

function playerNote(note, tempo) {
  const playerOctave = 3;
  playerSynth.triggerAttackRelease(
    setOctave(note, playerOctave),
    "16n",
    Tone.now(),
    Math.min(0.2 * tempo, 0.5));
}

function obstacleNote(note, tempo) {
  const obstacleOctave = 2;
  enemySynth.triggerAttackRelease(
    setOctave(note, obstacleOctave),
    "16n",
    Tone.now(),
    Math.min(0.2 * tempo, 0.5));
}

let spawns = 0;

app.ports.audioMsg.subscribe(({ message, tempo }) => {
  console.log(tempo);

  const note = nextNote(spawns);

  switch (message) {
    case "gameStarted":
      systemSynth.triggerAttackRelease("C4", "8n", undefined, 0.2);
      playerNote("G", tempo)
      obstacleNote("E", tempo)
      spawns = -2; // Offset spawn counter to keep player and spawns in sync
      break;

    case "playerJumped":
      playerNote(note, tempo)
      break;

    case "obstacleSpawned":
      obstacleNote(note, tempo)
      spawns += 1
      break;

    case "gameOver":
      systemSynth.triggerAttackRelease("C2", "16n", Tone.now(), 0.3);
      playerNote("D#", tempo)
      obstacleNote("C#", tempo)
      break;

    default:
      console.log(`Unknown Message: ${message}`);
      break;
  }
});
