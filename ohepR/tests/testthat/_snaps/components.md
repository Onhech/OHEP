# item row component renders expected structure

    <div class="item-row">
      <div class="item-label">Our organization creates value in the community in which it operates.</div>
      <div class="cell-score">3.89</div>
      <div class="sentiment-stack">
            <div class="segment bg-disagree" style="width: 5%;"></div>
            <div class="segment bg-neutral" style="width: 25%;">25%</div>
            <div class="segment bg-agree" style="width: 70%;">70%</div>
          </div>
      <div class="delta-container delta-industry"><span class="delta-pill dp-neg">-14%</span></div><div class="delta-container delta-prior"><span class="delta-pill dp-pos">+31%</span></div>
    </div>

# hero card component renders expected structure

    <div class="card fundamental-card">
      <div class="title-bar">
        <h2 class="card-title">Purpose</h2>
        <span class="status-pill status-good">Above Standard</span>
      </div>
      <div class="metrics-container">
        <div class="hero-row">
          <div>
            <div class="hero-label">Percentile</div>
            <div class="hero-value">76th</div>
          </div>
          <div class="delta-block positive">
            <div class="delta-icon">&#9650;</div>
            <div class="delta-details">
              <div class="delta-value">+24 pts</div>
              <div class="delta-year">vs. 2023</div>
            </div>
          </div>
        </div>
        <div class="sub-context">
          Score: <strong>4.03</strong>
          <span class="sub-delta">(+0.24)</span>
        </div>
      </div>
      <div class="band-wrap">
        <div class="tick-label" style="left:40%;">40</div>
        <div class="tick-label" style="left:50%;">50</div>
        <div class="tick-label" style="left:60%;">60</div>
        <div class="tick-label" style="left:85%;">85</div>
        <div class="band">
          <div class="z1"></div><div class="z2"></div><div class="z3"></div><div class="z4"></div>
          <div class="tick" style="left:40%;"></div><div class="tick" style="left:60%;"></div><div class="tick" style="left:85%;"></div>
        </div>
        <div class="midpoint"></div>
        <div class="marker" style="left:76%;"></div>
      </div>
      <div class="labels">
        <span>Area for Growth</span>
        <span>Industry standard</span>
        <span>Above standard</span>
        <span>Industry leader</span>
      </div>
    </div>

