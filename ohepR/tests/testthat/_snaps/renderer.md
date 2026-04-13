# renderer output is snapshot stable

    <style>#ohep-custom-dashboard, #ohep-custom-dashboard * { box-sizing: border-box; }
    #ohep-custom-dashboard.ohep-root {
      --bg-canvas: #F8FAFC;
      --bg-card: #FFFFFF;
      --bg-secondary: #F1F5F9;
      --border-light: #F1F5F9;
      --border-default: #E2E8F0;
      --border-band: #E5E7EB;
      --border-dark: #CBD5E1;
      --text-primary: #0F172A;
      --text-secondary: #334155;
      --text-tertiary: #475569;
      --text-muted: #64748B;
      --text-faint: #94A3B8;
      --text-decorative: #CBD5E1;
      --text-accent: #0D9488;
      --text-on-dark: #FFFFFF;
      --status-risk-bg: #FEF3C7;
      --status-risk-text: #92400E;
      --status-watch-bg: #FEF9C3;
      --status-watch-text: #854D0E;
      --status-good-bg: #DCFCE7;
      --status-good-text: #166534;
      --status-leader-bg: #CFFAFE;
      --status-leader-text: #155E75;
      --percentile-z1: #FCD34D;
      --percentile-z2: #B3D6DA;
      --percentile-z3: #0096A7;
      --percentile-z4: #395966;
      --outcome-risk-text: #D97706;
      --outcome-watch-text: #D97706;
      --outcome-good-text: #16A34A;
      --delta-pos-text: #15803D;
      --delta-pos-bg: #DCFCE7;
      --delta-neg-text: #EB7161;
      --delta-neg-bg: #FEE2E2;
      --delta-neu-text: #64748B;
      --delta-neu-bg: #F1F5F9;
      --favorability-agree: #0096A7;
      --favorability-neutral: #94A3B8;
      --favorability-disagree: #EB7161;
      --band-tick: #0F172A2E;
      --band-tick-label: #64748B;
      --band-midpoint: #94A3B8;
      --band-marker: #111827;
      --privacy-item-label: #94A3B8;
      --privacy-overlay-bg: rgba(255,255,255,0.85);
      --privacy-overlay-border: #CBD5E1;
      --privacy-overlay-text: #475569;
      --privacy-overlay-shadow: rgba(15,23,42,0.06);
      width: 1280px;
      height: 720px;
      background: var(--bg-canvas);
      padding: 24px 32px;
      border-radius: 8px;
      box-shadow: 0 10px 30px rgba(15, 23, 42, 0.1);
      display: flex;
      flex-direction: column;
      gap: 20px;
      overflow: hidden;
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
    }
    #ohep-custom-dashboard .top-row { display: flex; gap: 20px; height: 310px; flex-shrink: 0; }
    #ohep-custom-dashboard .bottom-row { flex: 1; display: flex; min-height: 0; }
    #ohep-custom-dashboard .card {
      background: var(--bg-card);
      border: 1px solid var(--border-default);
      border-radius: 12px;
      padding: 24px;
      box-shadow: 0 4px 12px rgba(15,23,42,.03);
      display: flex;
      flex-direction: column;
    }
    #ohep-custom-dashboard .fundamental-card { flex: 1.2; }
    #ohep-custom-dashboard .outcomes-card { flex: 1; }
    #ohep-custom-dashboard .item-card { width: 100%; padding: 20px 24px; }
    #ohep-custom-dashboard .title-bar { display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 16px; }
    #ohep-custom-dashboard .card-title { font-size: 22px; font-weight: 900; color: var(--text-primary); margin: 0; text-transform: uppercase; letter-spacing: -0.5px; }
    #ohep-custom-dashboard .status-pill { font-size: 11px; font-weight: 800; padding: 6px 12px; border-radius: 12px; text-transform: uppercase; letter-spacing: 0.5px; }
    #ohep-custom-dashboard .status-risk { background: var(--status-risk-bg); color: var(--status-risk-text); }
    #ohep-custom-dashboard .status-watch { background: var(--status-watch-bg); color: var(--status-watch-text); }
    #ohep-custom-dashboard .status-good { background: var(--status-good-bg); color: var(--status-good-text); }
    #ohep-custom-dashboard .status-leader { background: var(--status-leader-bg); color: var(--status-leader-text); }
    #ohep-custom-dashboard .color-risk { color: var(--outcome-risk-text); }
    #ohep-custom-dashboard .color-watch { color: var(--outcome-watch-text); }
    #ohep-custom-dashboard .color-good { color: var(--outcome-good-text); }
    #ohep-custom-dashboard .metrics-container { display: flex; flex-direction: column; margin-bottom: 16px; }
    #ohep-custom-dashboard .hero-row { display: flex; align-items: flex-start; gap: 20px; }
    #ohep-custom-dashboard .hero-label { font-size: 11px; font-weight: 800; color: var(--text-accent); text-transform: uppercase; letter-spacing: 1px; margin-bottom: 4px; }
    #ohep-custom-dashboard .hero-value { font-size: 48px; font-weight: 900; color: var(--text-primary); line-height: 1; letter-spacing: -1px; }
    #ohep-custom-dashboard .delta-block { display: flex; align-items: center; gap: 6px; margin-top: 16px; }
    #ohep-custom-dashboard .delta-block.positive { color: var(--delta-pos-text); }
    #ohep-custom-dashboard .delta-block.negative { color: var(--delta-neg-text); }
    #ohep-custom-dashboard .delta-block.neutral { color: var(--text-muted); }
    #ohep-custom-dashboard .delta-icon { font-size: 20px; line-height: 1; }
    #ohep-custom-dashboard .delta-details { display: flex; flex-direction: column; justify-content: center; }
    #ohep-custom-dashboard .delta-value { font-size: 14px; font-weight: 800; line-height: 1.1; }
    #ohep-custom-dashboard .delta-year { font-size: 10px; color: var(--text-faint); font-weight: 700; text-transform: uppercase; margin-top: 2px; }
    #ohep-custom-dashboard .sub-context { font-size: 13px; color: var(--text-muted); font-weight: 600; margin-top: 12px; }
    #ohep-custom-dashboard .sub-context strong { color: var(--text-primary); font-weight: 800; }
    #ohep-custom-dashboard .sub-delta { color: var(--delta-pos-text); font-weight: 800; }
    #ohep-custom-dashboard .band-wrap { position: relative; margin-top: auto; padding-top: 16px; }
    #ohep-custom-dashboard .band { position: relative; height: 26px; border-radius: 999px; overflow: hidden; display: flex; border: 1px solid var(--border-band); }
    #ohep-custom-dashboard .z1 { width: 40%; background: var(--percentile-z1); }
    #ohep-custom-dashboard .z2 { width: 20%; background: var(--percentile-z2); }
    #ohep-custom-dashboard .z3 { width: 25%; background: var(--percentile-z3); }
    #ohep-custom-dashboard .z4 { width: 15%; background: var(--percentile-z4); }
    #ohep-custom-dashboard .tick { position: absolute; top: 0; width: 2px; height: 26px; background: var(--band-tick); z-index: 2; transform: translateX(-50%); }
    #ohep-custom-dashboard .tick-label { position: absolute; top: 12px; transform: translate(-50%, -100%); font-size: 11px; font-weight: 800; color: var(--band-tick-label); }
    #ohep-custom-dashboard .midpoint { position: absolute; top: 29px; left: 50%; width: 2px; height: 32px; background: var(--band-midpoint); transform: translate(-50%, -50%); z-index: 3; }
    #ohep-custom-dashboard .marker { position: absolute; top: 29px; width: 4px; height: 38px; background: var(--band-marker); transform: translate(-50%, -50%); border-radius: 4px; z-index: 5; }
    #ohep-custom-dashboard .labels { display: flex; align-items: center; font-size: 11px; font-weight: 600; color: var(--text-muted); margin-top: 10px; }
    #ohep-custom-dashboard .labels span { display: flex; justify-content: center; text-align: center; }
    #ohep-custom-dashboard .labels span:nth-child(1) { width: 40%; }
    #ohep-custom-dashboard .labels span:nth-child(2) { width: 20%; font-weight: 800; color: var(--status-good-text); }
    #ohep-custom-dashboard .labels span:nth-child(3) { width: 25%; }
    #ohep-custom-dashboard .labels span:nth-child(4) { width: 15%; }
    #ohep-custom-dashboard .table-outcomes { width: 100%; border-collapse: collapse; margin-top: auto; }
    #ohep-custom-dashboard .table-outcomes th {
      text-align: left; padding-bottom: 10px; font-size: 10px; font-weight: 800; color: var(--text-faint);
      text-transform: uppercase; letter-spacing: 0.5px; border-bottom: 2px solid var(--border-default);
    }
    #ohep-custom-dashboard .table-outcomes th.right-align { text-align: right; }
    #ohep-custom-dashboard .table-outcomes td { padding: 14px 0; border-bottom: 1px solid var(--border-light); vertical-align: middle; }
    #ohep-custom-dashboard .table-outcomes tr:last-child td { border-bottom: none; padding-bottom: 0; }
    #ohep-custom-dashboard .row-rank { font-size: 22px; font-weight: 800; color: var(--text-decorative); margin-right: 12px; }
    #ohep-custom-dashboard .row-name { font-size: 15px; font-weight: 800; color: var(--text-primary); }
    #ohep-custom-dashboard .company-score-stack { display: flex; flex-direction: column; align-items: flex-end; }
    #ohep-custom-dashboard .row-score-val { font-size: 15px; font-weight: 800; line-height: 1; }
    #ohep-custom-dashboard .row-status { font-size: 10px; font-weight: 800; text-transform: uppercase; letter-spacing: 0.3px; margin-top: 4px; }
    #ohep-custom-dashboard .item-title-row { display: flex; justify-content: space-between; align-items: center; margin-bottom: 22px; }
    #ohep-custom-dashboard .legend-group { display: flex; gap: 16px; font-size: 11px; font-weight: 700; color: var(--text-muted); text-transform: uppercase; }
    #ohep-custom-dashboard .leg-item { display: flex; align-items: center; gap: 6px; }
    #ohep-custom-dashboard .leg-dot { width: 10px; height: 10px; border-radius: 2px; }
    #ohep-custom-dashboard .bg-agree { background: var(--favorability-agree); }
    #ohep-custom-dashboard .bg-neutral { background: var(--favorability-neutral); }
    #ohep-custom-dashboard .bg-disagree { background: var(--favorability-disagree); }
    #ohep-custom-dashboard .item-grid-2col { display: grid; grid-template-columns: 1fr 1fr; gap: 40px; flex: 1; }
    #ohep-custom-dashboard .item-column { display: flex; flex-direction: column; }
    #ohep-custom-dashboard .header-row, #ohep-custom-dashboard .item-row { display: grid; grid-template-columns: minmax(180px, 2fr) 40px 3fr 50px 50px; gap: 12px; align-items: center; }
    #ohep-custom-dashboard .header-row { padding-bottom: 8px; border-bottom: 2px solid var(--border-default); }
    #ohep-custom-dashboard .h-lbl { font-size: 9px; font-weight: 800; color: var(--text-faint); text-transform: uppercase; letter-spacing: 0.5px; text-align: center; }
    #ohep-custom-dashboard .h-lbl:first-child { text-align: left; }
    #ohep-custom-dashboard .sub-cat { grid-column: 1 / -1; background: var(--bg-secondary); font-size: 11px; font-weight: 800; color: var(--text-secondary); text-transform: uppercase; letter-spacing: 1px; padding: 6px 12px; border-radius: 4px; margin: 8px 0; }
    #ohep-custom-dashboard .item-row { padding: 6px 0; border-bottom: 1px solid var(--border-light); }
    #ohep-custom-dashboard .item-row:last-child { border-bottom: none; }
    #ohep-custom-dashboard .item-row > .item-label { grid-column: 1; }
    #ohep-custom-dashboard .item-row > .cell-score { grid-column: 2; }
    #ohep-custom-dashboard .item-row > .sentiment-stack { grid-column: 3; }
    #ohep-custom-dashboard .item-row > .sentiment-privacy-stack { grid-column: 3; display: flex; flex-direction: column; align-items: center; gap: 8px; }
    #ohep-custom-dashboard .item-row > .sentiment-privacy-stack > .sentiment-stack { width: 100%; }
    #ohep-custom-dashboard .item-row > .delta-industry { grid-column: 4; }
    #ohep-custom-dashboard .item-row > .delta-prior { grid-column: 5; }
    #ohep-custom-dashboard .item-label { font-size: 12px; font-weight: 600; color: var(--text-secondary); line-height: 1.3; padding-right: 8px; }
    #ohep-custom-dashboard .cell-score { font-size: 13px; font-weight: 800; color: var(--text-primary); text-align: center; }
    #ohep-custom-dashboard .sentiment-stack { height: 22px; display: flex; border-radius: 4px; overflow: hidden; background: var(--bg-secondary); }
    #ohep-custom-dashboard .segment { display: flex; align-items: center; justify-content: center; color: var(--text-on-dark); font-size: 10px; font-weight: 800; }
    #ohep-custom-dashboard .delta-container { display: flex; justify-content: center; }
    #ohep-custom-dashboard .delta-pill { display: inline-flex; align-items: center; justify-content: center; width: 48px; padding: 4px 0; border-radius: 4px; font-size: 10px; font-weight: 800; }
    #ohep-custom-dashboard .dp-pos { background: var(--delta-pos-bg); color: var(--delta-pos-text); }
    #ohep-custom-dashboard .dp-neg { background: var(--delta-neg-bg); color: var(--delta-neg-text); }
    #ohep-custom-dashboard .dp-neu { background: var(--delta-neu-bg); color: var(--delta-neu-text); }
    #ohep-custom-dashboard .privacy-row { position: relative; }
    #ohep-custom-dashboard .privacy-row .item-label { color: var(--privacy-item-label); }
    #ohep-custom-dashboard .privacy-row > .cell-score,
    #ohep-custom-dashboard .privacy-row > .delta-container,
    #ohep-custom-dashboard .privacy-row > .sentiment-stack,
    #ohep-custom-dashboard .privacy-row > .sentiment-privacy-stack > .sentiment-stack { filter: blur(4px) grayscale(60%); opacity: 0.25; pointer-events: none; user-select: none; }
    #ohep-custom-dashboard .privacy-overlay {
      position: relative;
      z-index: 3;
      background: var(--privacy-overlay-bg);
      backdrop-filter: blur(2px);
      border: 1px solid var(--privacy-overlay-border);
      border-radius: 6px;
      padding: 6px 14px;
      font-size: 11px;
      font-weight: 700;
      color: var(--privacy-overlay-text);
      letter-spacing: 0.2px;
      box-shadow: 0 2px 6px var(--privacy-overlay-shadow);
      display: flex;
      align-items: center;
      gap: 6px;
    }</style>
    <div id="ohep-custom-dashboard" class="ohep-root">
      <div class="top-row">
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
        <div class="card outcomes-card">
      <div class="title-bar">
        <h2 class="card-title">Purpose drives</h2>
      </div>
      <table class="table-outcomes">
        <thead>
          <tr>
            <th>Outcome</th>
            <th class="right-align">Your Company</th>
          </tr>
        </thead>
        <tbody><tr>
      <td>
        <span class="row-rank">1</span>
        <span class="row-name">Engagement</span>
      </td>
      <td>
        <div class="company-score-stack">
          <span class="row-score-val color-risk">28th Percentile</span>
          <span class="row-status color-risk">Area for Growth</span>
        </div>
      </td>
    </tr><tr>
      <td>
        <span class="row-rank">2</span>
        <span class="row-name">Burnout</span>
      </td>
      <td>
        <div class="company-score-stack">
          <span class="row-score-val color-risk">25th Percentile</span>
          <span class="row-status color-risk">Area for Growth</span>
        </div>
      </td>
    </tr><tr>
      <td>
        <span class="row-rank">3</span>
        <span class="row-name">Work Satisfaction</span>
      </td>
      <td>
        <div class="company-score-stack">
          <span class="row-score-val color-risk">24th Percentile</span>
          <span class="row-status color-risk">Area for Growth</span>
        </div>
      </td>
    </tr></tbody>
      </table>
    </div>
      </div>
      <div class="bottom-row">
        <div class="card item-card">
          <div class="item-title-row">
            <h2 class="card-title">Item-Level Breakdown</h2>
            <div class="legend-group">
              <div class="leg-item"><div class="leg-dot bg-disagree"></div> Disagree</div>
              <div class="leg-item"><div class="leg-dot bg-neutral"></div> Neutral</div>
              <div class="leg-item"><div class="leg-dot bg-agree"></div> Agree</div>
            </div>
          </div>
          <div class="item-grid-2col">
            <div class="item-column"><div class="header-row">
      <div class="h-lbl">Survey Item</div><div class="h-lbl">Mean</div><div class="h-lbl">Sentiment Distribution</div><div class="h-lbl">vs Ind.</div><div class="h-lbl">vs '24</div>
    </div><div class="sub-cat">Purpose</div><div class="item-row">
      <div class="item-label">Our organization creates value in the community in which it operates.</div>
      <div class="cell-score">3.89</div>
      <div class="sentiment-stack">
            <div class="segment bg-disagree" style="width: 5%;"></div>
            <div class="segment bg-neutral" style="width: 25%;">25%</div>
            <div class="segment bg-agree" style="width: 70%;">70%</div>
          </div>
      <div class="delta-container delta-industry"><span class="delta-pill dp-neg">-14%</span></div><div class="delta-container delta-prior"><span class="delta-pill dp-pos">+31%</span></div>
    </div><div class="item-row">
      <div class="item-label">Our organization builds and maintains quality relationships with all stakeholders.</div>
      <div class="cell-score">4.05</div>
      <div class="sentiment-stack">
            <div class="segment bg-disagree" style="width: 2%;"></div>
            <div class="segment bg-neutral" style="width: 20%;">20%</div>
            <div class="segment bg-agree" style="width: 78%;">78%</div>
          </div>
      <div class="delta-container delta-industry"><span class="delta-pill dp-neg">-10%</span></div><div class="delta-container delta-prior"><span class="delta-pill dp-pos">+30%</span></div>
    </div><div class="item-row">
      <div class="item-label">Our organization considers the needs of the community and society.</div>
      <div class="cell-score">3.72</div>
      <div class="sentiment-stack">
            <div class="segment bg-disagree" style="width: 5%;"></div>
            <div class="segment bg-neutral" style="width: 31%;">31%</div>
            <div class="segment bg-agree" style="width: 64%;">64%</div>
          </div>
      <div class="delta-container delta-industry"><span class="delta-pill dp-neg">-12%</span></div><div class="delta-container delta-prior"><span class="delta-pill dp-pos">+23%</span></div>
    </div></div>
            <div class="item-column"><div class="header-row">
      <div class="h-lbl">Survey Item</div><div class="h-lbl">Mean</div><div class="h-lbl">Sentiment Distribution</div><div class="h-lbl">vs Ind.</div><div class="h-lbl">vs '24</div>
    </div><div class="sub-cat">Purpose</div><div class="item-row">
      <div class="item-label">Our organization's purpose/mission makes me feel that my work is meaningful.</div>
      <div class="cell-score">4.03</div>
      <div class="sentiment-stack">
            <div class="segment bg-disagree" style="width: 5%;"></div>
            <div class="segment bg-neutral" style="width: 15%;">15%</div>
            <div class="segment bg-agree" style="width: 80%;">80%</div>
          </div>
      <div class="delta-container delta-industry"><span class="delta-pill dp-pos">+3%</span></div><div class="delta-container delta-prior"><span class="delta-pill dp-pos">+28%</span></div>
    </div><div class="item-row">
      <div class="item-label">I am proud to work for this organization.</div>
      <div class="cell-score">4.33</div>
      <div class="sentiment-stack">
            <div class="segment bg-disagree" style="width: 3%;"></div>
            <div class="segment bg-neutral" style="width: 10%;">10%</div>
            <div class="segment bg-agree" style="width: 87%;">87%</div>
          </div>
      <div class="delta-container delta-industry"><span class="delta-pill dp-neg">-3%</span></div><div class="delta-container delta-prior"><span class="delta-pill dp-pos">+25%</span></div>
    </div><div class="item-row">
      <div class="item-label">It is clear to me how my work connects to our broader organizational purpose.</div>
      <div class="cell-score">4.16</div>
      <div class="sentiment-stack">
            <div class="segment bg-disagree" style="width: 5%;"></div>
            <div class="segment bg-neutral" style="width: 13%;">13%</div>
            <div class="segment bg-agree" style="width: 82%;">82%</div>
          </div>
      <div class="delta-container delta-industry"><span class="delta-pill dp-neg">-3%</span></div><div class="delta-container delta-prior"><span class="delta-pill dp-pos">+9%</span></div>
    </div></div>
          </div>
        </div>
      </div>
    </div>

