import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  switch (color) {
    case "r": return "#FE3032";
    case "v": return "#671765";
    case "p": return "#F766B1";
    case "g": return "#119730";
    case "b": return "#26AFF1";
    case "y": return "#FBCC38";
  }
  return color;
}
class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      origen: "[0,0]",
      capturados : 0,
      history : [],
      turns: 0,
      grid: null,
      started: false,
      complete: false,  // true if game is complete, false otherwise
      waiting: false
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
  }

  handlePengineCreate() {
    const queryS = 'init(Grid)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid']
        });
      }
    });
  }

  assignOrigen(coords) {
    if (this.state.complete || this.state.waiting){
      return;
    }
    if (!this.state.started){
      this.setState({origen: coords});
      alert("Celda de origen asignada");
    }
  }

  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    if (this.state.complete || this.state.waiting) {
      return;
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
    //        [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
    //        [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
    //        [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
    //        [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
    //        [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
    //        [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
    //        [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
    //        [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
    //        [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
    //        [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
    //        [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
    //        [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],r, Grid)
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const queryS = "flick(" + gridS + ","+this.state.origen+"," + color + ", Grid, Capturados)";
    this.setState({
      waiting: true
    });
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid'],
          capturados: response['Capturados'],
          history: [color, ...this.state.history],
          turns: this.state.turns + 1,
          waiting: false
        });

        //Crea la consulta para ver si el juego se acabo
        const gridTest = JSON.stringify(this.state.grid).replaceAll('"', "");
        const queryTest = "gridComplete(" + gridTest + ","+this.state.capturados+")";
        this.pengine.query(queryTest, (success) => {
          if (success) {
            this.setState({
              complete : true
            })
            alert("Felicidades a ganado el juego");
          }
        })

        //Pregunta si ya comenzo el juego, sino lo hizo lo comienza
        if (this.state.started === false){
          this.setState({started : true});
        }
      } else {
        // Prolog query will fail when the clicked color coincides with that in the top left cell.
        this.setState({
          waiting: false
        });
      }
    });
  }

  render() {
    if (this.state.grid === null) {
      return null;
    }
    return (
      <div className="game">
        <div className="leftPanel">
          <div className="buttonsPanel">
            {colors.map(color =>
              <button
                className="colorBtn"
                style={{ backgroundColor: colorToCss(color) }}
                onClick={() => this.handleClick(color)}
                key={color}
              />)}
          </div>
          <div className="turnsPanel">
            <div className="turnsLab">Turns</div>
            <div className="turnsNum">{this.state.turns}</div>
          </div>
          <div className="capturedPanel">
            <div className="capturedLab">Capturados</div>
            <div className="capturedNum">{this.state.capturados}</div>
          </div>
          <div className="historyPanel">
            <div className="historyLab">Historial</div>
            <div className="historyScroll">
              {this.state.history.map(color =>
              <div
                className="colorCell"
                style={{ backgroundColor: colorToCss(color) }}
              />)}</div>
          </div>
        </div>
        <div className="container">
          <Board grid={this.state.grid} />
          <div className = "buttonMatrix">
                  {this.state.grid.map((row, i) =>
                      row.map((cell, j) =>
                      <button className="origenBtn"
                      value={cell}
                      onClick={() => this.assignOrigen("["+i+","+j+"]")}
                      key={i + "." + j}
                      ></button>
                      )
                      )}
          </div>
        </div>
        <div className="rightPanel">
          <h1>Color flick</h1>
          <div className="instructionsText">
              <p>Para jugar primero debe seleccionar la celda donde desea originar, una vez seleccione una celda esta no puede ser cambiada, de seleccionar una
              el juego le asigna la celda en la parte super izquierda de la grilla.</p>
              <p>El juego consta de ir seleccionado colores, estos colores cambian las celdas que son adjacentes y del mismo color a la celda origen, el juego se acaba
              cuando el jugador logra hacer que toda la grilla tenga el mismo color. A jugar.</p>
              </div>
        </div>
      </div>
    );
  }
}

export default Game;