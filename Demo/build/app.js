(function () {
  const data = window.OHEP_DEMO_DATA;
  if (!data) {
    document.body.innerHTML = "<p style='padding:16px;font-family:sans-serif'>Missing demo-data.js bundle.</p>";
    return;
  }

  const storageKey = "ohep_demo_shell_state_v3";
  const sidebarEl = document.getElementById("sidebar-nav");
  const frameEl = document.getElementById("slide-frame");
  const titleEl = document.getElementById("report-title");
  const filterGroupsEl = document.getElementById("filter-groups");
  const responsesEl = document.getElementById("global-responses-value");
  const filterToggle = document.getElementById("filter-toggle");
  const filterPopover = document.getElementById("filter-popover");
  const filterBadge = document.getElementById("filter-badge");
  const clearFiltersBtn = document.getElementById("clear-filters");
  const exportToggle = document.getElementById("export-toggle");
  const exportMain = document.getElementById("export-main");
  const exportMenu = document.getElementById("export-menu");
  const slideCountEl = document.getElementById("slide-count");
  const prevBtn = document.getElementById("prev-btn");
  const nextBtn = document.getElementById("next-btn");

  const slideOrder = data.slide_order || [];
  const slidesById = Object.fromEntries((data.slides || []).map((s) => [s.slide_id, s]));
  const dimensionDefs = data.filter_dimensions || [];
  const dims = Object.fromEntries(dimensionDefs.map((d) => [d.id, d.options || []]));
  const dimIds = dimensionDefs.map((d) => d.id);
  const filterEls = {};
  const sectionLabelMap = {
    Insights: "Themes",
    Comments: "Comment Explorer"
  };
  const slideLabelMap = {
    "Segment Heatmap": "Segment Analysis",
    "Themes Overview": "Overview",
    "Three Key Takeaways": "Key Takeaways",
    Comments: "Comment Explorer"
  };

  const defaults = {
    slideId: slideOrder[0]
  };
  dimIds.forEach(function (id) {
    defaults[id] = "All";
  });

  let state = loadState();
  if (!slidesById[state.slideId]) state.slideId = defaults.slideId;
  dimIds.forEach(function (id) {
    const opts = dims[id] || [];
    if (!opts.includes(state[id])) {
      state[id] = defaults[id];
    }
  });

  titleEl.textContent = data.meta?.client_label || "Arctic Slope";
  renderFilterControls();

  renderSidebar();
  renderShell();

  filterToggle.addEventListener("click", function (event) {
    event.stopPropagation();
    const nextHidden = !filterPopover.hidden;
    filterPopover.hidden = nextHidden;
    filterToggle.classList.toggle("active", !nextHidden);
    filterToggle.setAttribute("aria-expanded", String(!nextHidden));
    if (!nextHidden) {
      exportMenu.hidden = true;
      exportToggle.setAttribute("aria-expanded", "false");
    }
  });

  clearFiltersBtn.addEventListener("click", function () {
    dimIds.forEach(function (id) {
      state[id] = defaults[id];
      if (filterEls[id]) {
        renderSelect(filterEls[id], dims[id], state[id]);
      }
    });
    renderShell();
    persistState();
  });

  exportToggle.addEventListener("click", function (event) {
    event.stopPropagation();
    exportMenu.hidden = !exportMenu.hidden;
    exportToggle.setAttribute("aria-expanded", String(!exportMenu.hidden));
    filterPopover.hidden = true;
    filterToggle.classList.remove("active");
    filterToggle.setAttribute("aria-expanded", "false");
  });

  exportMain.addEventListener("click", function () {
    exportMenu.hidden = false;
    exportToggle.setAttribute("aria-expanded", "true");
  });

  prevBtn.addEventListener("click", function () {
    const idx = Math.max(0, slideOrder.indexOf(state.slideId) - 1);
    state.slideId = slideOrder[idx];
    renderSidebar();
    renderShell();
    persistState();
  });

  nextBtn.addEventListener("click", function () {
    const idx = Math.min(slideOrder.length - 1, slideOrder.indexOf(state.slideId) + 1);
    state.slideId = slideOrder[idx];
    renderSidebar();
    renderShell();
    persistState();
  });

  document.addEventListener("click", function (event) {
    if (!filterPopover.hidden && !filterPopover.contains(event.target) && !filterToggle.contains(event.target)) {
      filterPopover.hidden = true;
      filterToggle.classList.remove("active");
      filterToggle.setAttribute("aria-expanded", "false");
    }
    if (!exportMenu.hidden && !exportMenu.contains(event.target) && !exportToggle.contains(event.target) && !exportMain.contains(event.target)) {
      exportMenu.hidden = true;
      exportToggle.setAttribute("aria-expanded", "false");
    }
  });

  document.addEventListener("keydown", function (event) {
    const tag = document.activeElement && document.activeElement.tagName;
    if (tag === "INPUT" || tag === "SELECT" || tag === "TEXTAREA") return;
    if (event.key === "ArrowLeft" && !prevBtn.disabled) prevBtn.click();
    if (event.key === "ArrowRight" && !nextBtn.disabled) nextBtn.click();
  });

  function onFilterChange(dimId) {
    if (filterEls[dimId]) {
      state[dimId] = filterEls[dimId].value;
    }
    renderShell();
    persistState();
  }

  function renderFilterControls() {
    filterGroupsEl.innerHTML = "";
    dimensionDefs.forEach(function (dim) {
      const wrap = document.createElement("label");
      wrap.className = "filter-group";
      const lbl = document.createElement("span");
      lbl.className = "filter-lbl";
      lbl.textContent = dim.label || dim.id;
      const sel = document.createElement("select");
      sel.className = "popover-select";
      sel.id = "filter-" + dim.id;
      renderSelect(sel, dim.options || ["All"], state[dim.id]);
      sel.addEventListener("change", function () {
        onFilterChange(dim.id);
      });
      filterEls[dim.id] = sel;
      wrap.appendChild(lbl);
      wrap.appendChild(sel);
      filterGroupsEl.appendChild(wrap);
    });
  }

  function renderShell() {
    const filterId = makeFilterId();
    const key = state.slideId + "::" + filterId;
    frameEl.srcdoc = data.slide_html?.[key] || data.no_data_html || "<p>No data.</p>";

    const responseCount = data.responses_by_filter?.[filterId] || 0;
    responsesEl.textContent = String(responseCount);

    const activeFilters = dimIds.filter(function (id) {
      return state[id] !== "All";
    }).length;
    filterBadge.textContent = String(activeFilters);

    const current = Math.max(0, slideOrder.indexOf(state.slideId)) + 1;
    slideCountEl.textContent = current + " / " + slideOrder.length;
    prevBtn.disabled = current <= 1;
    nextBtn.disabled = current >= slideOrder.length;
  }

  function renderSidebar() {
    sidebarEl.innerHTML = "";
    const activeSectionId = slidesById[state.slideId]?.section_id || "";

    (data.sections || []).forEach((section) => {
      const sectionSlides = (section.slide_ids || []).map((id) => slidesById[id]).filter(Boolean);
      if (sectionSlides.length === 1 && (sectionSlides[0].slide_label || "").toLowerCase() === (section.section_label || "").toLowerCase()) {
        const btn = document.createElement("button");
        btn.type = "button";
        btn.className = "nav-section-link";
        if (sectionSlides[0].slide_id === state.slideId) btn.classList.add("active");
        btn.textContent = displaySectionLabel(section.section_label);
        btn.addEventListener("click", function () {
          state.slideId = sectionSlides[0].slide_id;
          renderSidebar();
          renderShell();
          persistState();
        });
        sidebarEl.appendChild(btn);
        return;
      }

      const details = document.createElement("details");
      if (section.section_id === activeSectionId || section.section_id === "system_orientation") details.open = true;

      const summary = document.createElement("summary");
      summary.textContent = displaySectionLabel(section.section_label);
      details.appendChild(summary);

      const group = document.createElement("div");
      group.className = "nav-group";

      let currentGroup = "";
      sectionSlides.forEach((slide) => {
        const navGroup = slide.nav_group || "";
        if (navGroup && navGroup !== currentGroup) {
          currentGroup = navGroup;
          const subgroup = document.createElement("div");
          subgroup.className = "nav-subgroup";
          subgroup.textContent = navGroup;
          group.appendChild(subgroup);
        }

        const btn = document.createElement("button");
        btn.type = "button";
        btn.className = "nav-item level-" + String(slide.nav_level || 0);
        if (slide.slide_id === state.slideId) btn.classList.add("active");
        btn.textContent = displaySlideLabel(slide.slide_label);
        btn.addEventListener("click", function () {
          state.slideId = slide.slide_id;
          renderSidebar();
          renderShell();
          persistState();
        });
        group.appendChild(btn);
      });

      details.appendChild(group);
      sidebarEl.appendChild(details);
    });
  }

  function renderSelect(el, options, selected) {
    el.innerHTML = "";
    (options || []).forEach((option) => {
      const node = document.createElement("option");
      node.value = option;
      node.textContent = option;
      node.selected = option === selected;
      el.appendChild(node);
    });
  }

  function loadState() {
    try {
      const raw = localStorage.getItem(storageKey);
      if (!raw) return { ...defaults };
      return { ...defaults, ...JSON.parse(raw) };
    } catch (err) {
      return { ...defaults };
    }
  }

  function persistState() {
    localStorage.setItem(storageKey, JSON.stringify(state));
  }

  function makeFilterId() {
    return dimIds
      .map(function (id) {
        return id + "=" + slug(state[id]);
      })
      .join("|");
  }

  function slug(value) {
    return String(value || "")
      .toLowerCase()
      .trim()
      .replace(/[^a-z0-9]+/g, "_")
      .replace(/^_+|_+$/g, "")
      .replace(/_+/g, "_");
  }

  function displaySectionLabel(label) {
    return sectionLabelMap[label] || label;
  }

  function displaySlideLabel(label) {
    return slideLabelMap[label] || label;
  }
})();
