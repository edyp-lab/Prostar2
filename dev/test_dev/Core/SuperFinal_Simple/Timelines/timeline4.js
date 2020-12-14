class App {
  
 select = e => document.querySelector(e);
 selectAll = e => document.querySelectorAll(e); 
 mainTl = new TimelineMax();
 maxDragX = 480;
 num = 12;
 step = Math.abs(this.maxDragX/100);
 snap = Math.abs(this.maxDragX/this.num); 
 dragger = this.select('#dragger');
 boxTl = new TimelineMax({paused: true});
 numberTl = new TimelineMax({paused: true});
 trackTl = new TimelineMax({paused: true});
 track = this.select('#track');
 
  constructor(){
       
   const allBoxes = this.selectAll('#boxGroup rect');
   const allDividers = this.selectAll('#dividerGroup line');
   const allNumbers = this.selectAll('#numberGroup path');
   const cover = this.select('#cover');
   
   const introTl = new TimelineMax({delay: 1});
   this.mainTl.add(introTl);

   TweenMax.set(this.track, {
    attr:{
     x2: 188
    }
   });
   
   introTl.from('#trackBg', 1, {
    attr:{
     x1: 400,
     x2: 400
    },
    alpha: 1,
    ease: Circ.easeInOut
   })
   .staggerFrom('.endBox', 0.5, {
    cycle: {
     x:[ 20, -20 ]
    },
    transformOrigin: '50% 50%',
    scale: 0,
    ease: Power2.easeInOut
   }, 0,'-=0.5')
   .staggerFrom(allBoxes, 0.5, {
    scale: 0,
    ease: Sine.easeInOut,
    transformOrigin: '50% 50%'
   }, 0, '-=0.75')
   .staggerFrom(allDividers, 0.5, {
    attr:{     
     y2: '-=5'
    },
    alpha: 0,
    ease:Sine.easeOut
   }, 0, '-=0.5')      
   .staggerFrom('#numberGroupBg path', 0.6, {
    y: 6,
    alpha: 0
   }, 0.006, 0.5).from('.preservationText', 0.6, {
    alpha: 0,
    y:10,
    ease:Sine.easeInOut
   }, '-=0.8')
   
   this.trackTl.to(this.track, 1, {
    attr:{
     x2:188
    }
   }).to(this.track, 1, {
    attr:{
     x2: 220
    }
   }).to(this.track, 1, {
    attr:{
     x2: 260
    }
   }).to(this.track, 1, {
    attr:{
     x2:300
    }
   }).to(this.track, 1, {
    attr:{
     x2: 340
    }
   }).to(this.track, 1, {
    attr:{
     x2: 380
    }
   }).to(this.track, 1, {
    attr:{
     x2:420
    }
   }).to(this.track, 1, {
    attr:{
     x2: 460
    }
   }).to(this.track, 1, {
    attr:{
     x2: 500
    }
   }).to(this.track, 1, {
    attr:{
     x2: 540
    }
   }).to(this.track, 1, {
    attr:{
     x2: 580
    }
   }).to(this.track, 1, {
    attr:{
     x2: 613
    }
   })
   
   this.boxTl.staggerFrom(allBoxes, 1, {
    fillOpacity: 0,
    attr:{
     width: 6,
     height: 6,
    },
     x: 10, 
     y: 10,
    transformOrigin: '50% 50%',
    ease:Sine.easeInOut
   }, 1)
   
   this.numberTl.staggerFrom(allNumbers, 1, {
    alpha: 0,
    ease:Sine.easeInOut
   }, 1)
   
   Draggable.create(this.dragger, {
    trigger: cover,
    type: 'x',
    cursor: 'ew-resize',
    maxDuration: 3,
    throwResistance: 1000, 
    bounds: {maxX:this.maxDragX, minX:0},
    snap: (value) => {
      return Math.round(value/this.snap) * this.snap
     },
    throwProps: true,
    onDrag: this.onUpdate,
    onThrowUpdate: this.onUpdate,
    overshootTolerance: 0
   });    

   document.addEventListener("touchmove", (e) => {
      e.preventDefault();
  });
  }
 
  onUpdate = () => {
    let pos = this.dragger._gsTransform.x / (this.maxDragX);
   this.numberTl.progress(pos);
   this.boxTl.progress(pos);
   this.trackTl.progress(pos);
  }
 
  get timeline():boolean {
    return this.mainTl;
  }
 
}


TweenMax.set('svg', {
  visibility: 'visible'
})

var app = new App();

