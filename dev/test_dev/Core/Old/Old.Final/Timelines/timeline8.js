const { useState, useEffect } = React;

const timeLineBalls = (n, onClick, current) => (
  Array(n).fill(0).map((i, index) => (
    <div key={index} 
    className={`timeline__ball ${current >= index ? 'active' : null}`} 
    onClick={() => onClick(index)} >
      {index + 1}
    </div>
  ))
);

const App = () => {
  const [width, setWidth] = useState(0);
  const [shouldIncrement, setShouldIncrement] = useState(true)
  const intermediaryBalls = 5;
  const calculatedWidth = (width / (intermediaryBalls + 1) ) * 100
  return (
    <div className="timeline">
      <div className="timeline__progress" style={{ width: `${calculatedWidth}%` }}
      >
      {timeLineBalls(intermediaryBalls + 2, setWidth, width)}
    </div>
    </div>
  );
}

ReactDOM.render(<App />,
document.getElementById("root"));