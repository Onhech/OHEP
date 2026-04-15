(function () {
  const data = window.OHEP_DEMO_DATA;
  if (!data) {
    document.body.innerHTML = "<p style='padding:16px;font-family:sans-serif'>Missing demo-data.js bundle.</p>";
    return;
  }

  const storageKey = "ohep_demo_shell_state_v2";
  const sidebarEl = document.getElementById("sidebar");
  const frameEl = document.getElementById("slide-frame");
  const titleEl = document.getElementById("report-title");
  const subtitleEl = document.getElementById("report-subtitle");
  const depEl = document.getElementById("filter-department");
  const statusEl = document.getElementById("filter-full-time-status");
  const locEl = document.getElementById("filter-location-cluster");
  const slideCountEl = document.getElementById("slide-count");
  const prevBtn = document.getElementById("prev-btn");
  const nextBtn = document.getElementById("next-btn");

  const slideOrder = data.slide_order || [];
  const slidesById = Object.fromEntries((data.slides || []).map((s) => [s.slide_id, s]));
  const dims = Object.fromEntries((data.filter_dimensions || []).map((d) => [d.id, d.options || []]));

  const defaults = {
    slideId: slideOrder[0],
    department: "All",
    full_time_status: "All",
    location_cluster: "All"
  };

  let state = loadState();
  if (!slidesById[state.slideId]) state.slideId = defaults.slideId;
  if (!dims.department.includes(state.department)) state.department = "All";
  if (!dims.full_time_status.includes(state.full_time_status)) state.full_time_status = "All";
  if (!dims.location_cluster.includes(state.location_cluster)) state.location_cluster = "All";

  titleEl.textContent = data.meta?.report_title || "OHEP Demo Report";
  subtitleEl.textContent = data.meta?.report_subtitle || "";

  renderFilterOptions(depEl, dims.department, state.department);
  renderFilterOptions(statusEl, dims.full_time_status, state.full_time_status);
  renderFilterOptions(locEl, dims.location_cluster, state.location_cluster);
  renderSidebar();
  renderSlide();

  depEl.addEventListener("change", () => {
    state.department = depEl.value;
    renderSlide();
    persistState();
  });
  statusEl.addEventListener("change", () => {
    state.full_time_status = statusEl.value;
    renderSlide();
    persistState();
  });
  locEl.addEventListener("change", () => {
    state.location_cluster = locEl.value;
    renderSlide();
    persistState();
  });

  prevBtn.addEventListener("click", () => {
    const idx = Math.max(0, slideOrder.indexOf(state.slideId) - 1);
    state.slideId = slideOrder[idx];
    renderSidebar();
    renderSlide();
    persistState();
  });

  nextBtn.addEventListener("click", () => {
    const idx = Math.min(slideOrder.length - 1, slideOrder.indexOf(state.slideId) + 1);
    state.slideId = slideOrder[idx];
    renderSidebar();
    renderSlide();
    persistState();
  });

  function loadState() {
    try {
      const raw = localStorage.getItem(storageKey);
      if (!raw) return { ...defaults };
      const parsed = JSON.parse(raw);
      return {
        slideId: parsed.slideId || defaults.slideId,
        department: parsed.department || defaults.department,
        full_time_status: parsed.full_time_status || defaults.full_time_status,
        location_cluster: parsed.location_cluster || defaults.location_cluster
      };
    } catch (err) {
      return { ...defaults };
    }
  }

  function persistState() {
    localStorage.setItem(storageKey, JSON.stringify(state));
  }

  function renderFilterOptions(el, options, selected) {
    el.innerHTML = "";
    (options || []).forEach((opt) => {
      const o = document.createElement("option");
      o.value = opt;
      o.textContent = opt;
      if (opt === selected) o.selected = true;
      el.appendChild(o);
    });
  }

  function renderSidebar() {
    sidebarEl.innerHTML = "";
    (data.sections || []).forEach((section) => {
      const block = document.createElement("section");
      block.className = "sidebar-section";
      const h = document.createElement("h3");
      h.textContent = section.section_label;
      block.appendChild(h);

      (section.slide_ids || []).forEach((slideId) => {
        const slide = slidesById[slideId];
        if (!slide) return;
        const btn = document.createElement("button");
        btn.type = "button";
        btn.className = "nav-item";
        if (slideId === state.slideId) btn.classList.add("active");
        btn.textContent = slide.slide_label;
        btn.addEventListener("click", () => {
          state.slideId = slideId;
          renderSidebar();
          renderSlide();
          persistState();
        });
        block.appendChild(btn);
      });

      sidebarEl.appendChild(block);
    });
  }

  function renderSlide() {
    const filterId = makeFilterId();
    const key = `${state.slideId}::${filterId}`;
    const html = data.slide_html?.[key] || data.no_data_html || "<p>No data.</p>";
    frameEl.srcdoc = html;

    const current = slideOrder.indexOf(state.slideId) + 1;
    slideCountEl.textContent = `Slide ${current} / ${slideOrder.length}`;
    prevBtn.disabled = current <= 1;
    nextBtn.disabled = current >= slideOrder.length;
  }

  function makeFilterId() {
    return `department=${slug(state.department)}|status=${slug(state.full_time_status)}|location=${slug(state.location_cluster)}`;
  }

  function slug(x) {
    return String(x || "")
      .toLowerCase()
      .trim()
      .replace(/[^a-z0-9]+/g, "_")
      .replace(/^_+|_+$/g, "")
      .replace(/_+/g, "_");
  }
})();
