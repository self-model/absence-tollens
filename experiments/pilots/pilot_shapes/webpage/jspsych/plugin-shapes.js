
var jsShapes = (function (jspsych) {
  "use strict"; // what is this

    const info = {
      name: 'jsShapes',
      parameters: {
        left_shape: {
          type: jspsych.ParameterType.STRING,
          pretty_name: "Left shape",
          description: "The shape shown on the left. Either circle, not-circle or occluded",
          default: ['circle', 'not-circle']
        },
        right_shape: {
          type: jspsych.ParameterType.STRING,
          pretty_name: "Right shape",
          description: "The shape shown on the right. Either triangle, not-triangle or occluded",
          default: ['triangle', 'not-triangle']
        },
        occluder: {
          type: jspsych.ParameterType.STRING,
          pretty_name: "Occluder",
          description: "Red curtain that hides one of the shapes. Either on the left or right",
          default: ['left','right']
        },
        choices: {
          type: jspsych.ParameterType.STRING,
          pretty_name: "Choices",
          description: "Choice keys. The first one corresponds to the option on the left and the other one on the right.",
          default: ['s','g']
        },
        post_click_delay: {
          type: jspsych.ParameterType.INT,
          pretty_name: "Post click delay",
          default: 200,
          description: "Time to display choice before moving to the next screen (ms)"
        },
        min_conf_time: {
          type: jspsych.ParameterType.INT,
          pretty_name: "Minimum confidence time",
          default: 200,
          description: "Minimum time to rate confidence (to avoid hasty responding)"
        }
      }
    }

  /**
   * **PLUGIN-NAME**
   *
   * SHORT PLUGIN DESCRIPTION
   *
   * @author
   * @see {@link https://DOCUMENTATION_URL DOCUMENTATION LINK TEXT}
   */

  class JsShapes {
    constructor(jsPsych) {
      this.jsPsych = jsPsych;
    }

    trial(display_element, trial) {

      //open a p5 sketch
      let sketch = (p) => {

          // const du = p.min([window.innerWidth, window.innerHeight, 600])*7/10 //drawing unit

         // choices 'press S' and 'press G'
          var text_choices = () => {
          p.textFont('Noto Sans Mono');
          p.push()
          p.textSize(16)
          p.fill('white')
          p.translate(window.innerWidth*(11/25),600)
          p.text('press S',0,0)
          p.translate(window.innerWidth*(3/25),0)
          p.text('press G',0,0)
          p.pop()
          };

          // array of shapes
          var draw_shapes = [
            () => { p.circle(0,0,90);},
            () => { p.triangle(-40,40,0,-40,40,40);},
            () => { p.quad(-50,0,0,-50,50,0,0,50);},
            () => { polygon(0,0,50,5);},
            () => { p.square(0,0,80);}, //rectmode affects square too
          ]

          function polygon(x, y, radius, npoints) {
            var angle = p.TWO_PI / npoints;
            p.beginShape();
            for (var a = 0; a < p.TWO_PI; a += angle) {
              var sx = x + p.cos(a) * radius;
              var sy = y + p.sin(a) * radius;
              p.vertex(sx, sy);
            }
            p.endShape(p.CLOSE);}

          // pick which shapes to draw on left and right (stimuli)
          let left_shape_index;
          if (trial.left_shape === 'circle'){
            left_shape_index = 0;
          } else if (trial.left_shape === 'not-circle'){
            left_shape_index = Math.floor(Math.random()*4) + 1
          }

          let right_shape_index;
          if (trial.right_shape === 'triangle'){
            right_shape_index = 1;
          } else if (trial.right_shape === 'not-triangle'){
            var noTriangleArray = [0,2,3,4];
            var randomIndex = Math.floor(Math.random()*noTriangleArray.length);
            right_shape_index = noTriangleArray[randomIndex];
          }

          const shape_indices = [left_shape_index, right_shape_index];

          // pick which shapes to show as options to choose from for response
          let options = [];
          if (trial.occluder === 'right') {
            options[0] = 1; // triangle
            var noTriangleArray = [0,2,3,4];
            var random_index_option = Math.floor(Math.random()*noTriangleArray.length);
            options[1] = noTriangleArray[random_index_option];
          } else if (trial.occluder === 'left') {
            options[0] = 0; // circle
            var noCircleArray = [1,2,3,4];
            var random_index_option = Math.floor(Math.random()*noCircleArray.length);
            options[1] = noCircleArray[random_index_option];
          };
          if (Math.random() < 0.5) {
            options = [options[1], options[0]]
          };

          // add constraint for presented shape to never appear as an option? this might be nice for MT 

          // function to draw the options to choose from
          function draw_choices(response){
            p.push()
            p.translate(window.innerWidth*(11/25),550) // these are ridiculous numbers
            if (response==0) {
              p.strokeWeight(2)
              p.fill(p.color('#d7d8d9')) // alternative: #c7def0
            }
            p.scale(0.7)
            draw_shapes[options[0]]();
            p.pop()

            p.push()
            p.translate(window.innerWidth*(14/25),550)
            if (response==1) {
              p.strokeWeight(2)
              p.fill(p.color('#d7d8d9'))
            }
            p.scale(0.7)
            draw_shapes[options[1]]();
            p.pop()
          }

          // confidence ratings
          var rate_confidence = (confidence) => {

            p.background(128);

            window.dial_position = p.max(p.min(p.mouseY,window.innerHeight*3/4),window.innerHeight/4);
            // draw scale
            p.push()
            p.stroke(0);
            p.strokeWeight(4);
            p.line(window.innerWidth/2, window.innerHeight/4, window.innerWidth/2, window.innerHeight*3/4)
            p.pop()

            // add labels
            p.push()
            p.textAlign(p.LEFT)
            p.textSize(30)
            p.textFont('Quicksand');
            p.text('100% certain',window.innerWidth/2+40,window.innerHeight/4)
            p.text('Guessing',window.innerWidth/2+40,window.innerHeight*3/4)
            p.pop()

            if (window.mouseMoved) {
              // draw dial
              p.push()
              p.stroke(0);
              p.strokeWeight(0.5);
              p.fill(255)
              p.ellipse(window.innerWidth/2,window.dial_position,20)
              p.pop()
            }

          }

          //sketch setup
          p.setup = () => {
            let canvas = p.createCanvas(window.innerWidth, window.innerHeight);
            canvas.style('display','block')
            p.fill(255); // white
            p.background(128); // grey
            p.textFont('Noto Sans Mono');
            p.textAlign(p.CENTER, p.CENTER);
            p.rectMode(p.CENTER)
            trial.response = NaN;
            trial.RT = Infinity;
            window.confidence=-1;
            window.mouseMoved=false;
            window.start_time = p.millis()
          };

          // draw
          p.draw = () => {
            if (p.millis()-window.start_time < trial.RT + trial.post_click_delay) {
              p.background(128);
              p.fill('white')
              p.stroke('black')

              // draw frame
              p.push()
              p.stroke('white')
              p.strokeWeight(2)
              // p.noFill()
              p.fill(64)
              p.rect(window.innerWidth/2,250,650,350)
              p.pop()

              // draw left shape
              p.push()
              p.translate(window.innerWidth*2/5, 200)
              draw_shapes[left_shape_index]();

              // draw right shape
              p.translate(window.innerWidth*1/5, 0)
              draw_shapes[right_shape_index]();
              p.pop()

              // draw occluder
              p.push()
              p.fill('red')
              p.stroke('red')
              if (trial.occluder === 'left'){
                p.rect(window.innerWidth*2/5,200,125,125)
              } else if (trial.occluder === 'right'){
                p.rect(window.innerWidth*3/5,200,125,125)
              }
              p.pop()

              // question text
              p.push()
              p.textAlign(p.CENTER)
              p.textSize(20)
              p.textStyle(p.ITALIC)
              p.translate(window.innerWidth/2,380)
              p.text('What shape is hidden behind the red curtain?',0,0)
              p.pop()

              // response choices: two shapes, with time delay
              if (p.millis()-window.start_time > 1000){
                draw_choices(trial.response)
                text_choices()

                // and hint:
                p.push()
                p.textAlign(p.CENTER)
                p.textSize(16)
                // p.noStroke()
                // p.fill('black')
                p.translate(window.innerWidth/2,650)
                p.text('hint: if the left shape is a circle, the right shape is a triangle',0,0)
                p.pop()
              }


            // rate confidence
            } else if (window.confidence==-1) {
              window.trial_part = 'rating confidence';
              console.log('a')
              rate_confidence(window.confidence)

            // end trial
            } else {
              p.remove()
              this.jsPsych.finishTrial(window.trial_data);
            }
          }

          // trial data saving
          p.keyReleased = () => {
              var key_code = p.keyCode
              var key = String.fromCharCode(key_code).toLowerCase();
              if ((key=='g' | key=='s') & trial.RT==Infinity) {
                  trial.response = key=='s'? 0:1; // if key pressed is 's' trial response = 0, otherwise 1
                  trial.RT = p.millis()-window.start_time;
                  window.trial_data = {
                      shapes: shape_indices,
                      occluder: trial.occluder,
                      options: options,
                      response: trial.response,
                      RT: trial.RT
                  };
              }
          }

          // confidence data saving
          p.mouseClicked = () => {
            if (p.millis()-window.start_time > trial.RT + trial.post_click_delay + trial.min_conf_time) {
              window.confidence=1-((window.dial_position-window.innerHeight/4)/(window.innerHeight/2));
              window.trial_data.confidence=window.confidence
              window.trial_data.confidence_RT = p.millis()-window.start_time-trial.RT;
            }
          }

          p.mouseMoved = () => {
            if (window.trial_part=='rating confidence') {
              window.mouseMoved=true;
            }
          }

      };

      let myp5 = new p5(sketch);
      }
    }
  JsShapes.info = info;

  return JsShapes;
})(jsPsychModule);