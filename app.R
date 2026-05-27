library(shiny)
library(png)
library(ranger)
library(EBImage)
library(jpeg)

options(shiny.maxRequestSize = 100 * 1024^2)

`%||%` <- function(a, b) if (!is.null(a)) a else b

app_css <- "
body { background: #f6f7fb; }
.small-muted { color: #6c757d; font-size: 0.95rem; }
.card {
  background: #ffffff;
  border: 1px solid rgba(0,0,0,0.08);
  border-radius: 12px;
  padding: 14px 14px 10px 14px;
  margin-bottom: 12px;
}

/* Status badges: dark grey before completion, light green when complete, black text */
.badge {
  display: inline-block;
  padding: 6px 10px;
  border-radius: 999px;
  font-size: 0.95rem;
  border: 1px solid rgba(0,0,0,0.18);
  margin-right: 8px;
  margin-bottom: 8px;
  background: #cfcfcf;
  color: #000;
}
.badge.ok {
  background: #cfead6;
  border-color: rgba(25,135,84,0.40);
  color: #000;
}
.badge.warn {
  background: #ffe69c;
  border-color: rgba(255,193,7,0.55);
  color: #000;
}
.badge.off {
  background: #c7c7c7;
  border-color: rgba(0,0,0,0.20);
  color: #000;
}

.metric {
  background: rgba(13,110,253,0.08);
  border: 1px solid rgba(13,110,253,0.20);
  padding: 10px 12px;
  border-radius: 12px;
  font-size: 1.05rem;
}

/* Magnifier (loupe) */
#magnifier_glass {
  position: absolute;
  display: none;
  border: 2px solid rgba(0,0,0,0.25);
  border-radius: 50%;
  width: 160px;
  height: 160px;
  box-shadow: 0 8px 24px rgba(0,0,0,0.25);
  pointer-events: none;
  background-color: white;
  background-repeat: no-repeat;
  z-index: 1000;
}
"

magnifier_js <- "
(function(){
  let settings = { enabled: true, zoom: 4, size: 160 };

  let mouseDown = false;
  let dragStart = null;
  let dragging = false;

  let lastPos = null;      // {x,y} in client coords
  let reshowTimer = null;

  let observerAttached = false;

  function clamp(v, lo, hi){ return Math.max(lo, Math.min(hi, v)); }
  function getEl(id){ return document.getElementById(id); }

  function hideGlass(){
    const g = getEl('magnifier_glass');
    if (g) g.style.display = 'none';
  }

  function scheduleReshow(delay){
    if (reshowTimer) clearTimeout(reshowTimer);
    reshowTimer = setTimeout(function(){
      if (!settings.enabled) return;
      if (!lastPos) return;
      updateGlassAt(lastPos.x, lastPos.y, true);
    }, delay || 90);
  }

  function attachObserver(){
    if (observerAttached) return;
    const plot = getEl('img_plot');
    if (!plot) return;

    const obs = new MutationObserver(function(mutations){
      let shouldRefresh = false;
      for (const m of mutations){
        if (m.type === 'attributes' && m.attributeName === 'src'){
          shouldRefresh = true; break;
        }
        if (m.type === 'childList' && m.addedNodes && m.addedNodes.length > 0){
          for (const n of m.addedNodes){
            if (n && n.tagName === 'IMG') { shouldRefresh = true; break; }
            if (n && n.querySelector && n.querySelector('img')) { shouldRefresh = true; break; }
          }
          if (shouldRefresh) break;
        }
      }
      if (shouldRefresh){
        hideGlass();
        scheduleReshow(120);
      }
    });

    obs.observe(plot, {
      subtree: true,
      childList: true,
      attributes: true,
      attributeFilter: ['src']
    });

    observerAttached = true;
  }

  function updateGlassAt(cx, cy){
    attachObserver();

    const wrap = getEl('img_plot_wrap');
    const plot = getEl('img_plot');
    const g = getEl('magnifier_glass');
    if (!wrap || !plot || !g) return;

    if (!settings.enabled){
      g.style.display = 'none';
      return;
    }
    if (dragging){
      g.style.display = 'none';
      return;
    }

    const img = plot.querySelector('img');
    if (!img || !img.src){
      g.style.display = 'none';
      return;
    }

    const imgRect = img.getBoundingClientRect();
    const wrapRect = wrap.getBoundingClientRect();

    if (cx < imgRect.left || cx > imgRect.right || cy < imgRect.top || cy > imgRect.bottom){
      g.style.display = 'none';
      return;
    }

    const zoom = Number(settings.zoom) || 4;
    const size = Number(settings.size) || 160;
    const half = size / 2;

    let x = cx - imgRect.left;
    let y = cy - imgRect.top;

    const minX = half / zoom;
    const maxX = imgRect.width  - half / zoom;
    const minY = half / zoom;
    const maxY = imgRect.height - half / zoom;
    x = clamp(x, minX, maxX);
    y = clamp(y, minY, maxY);

    g.style.display = 'block';
    g.style.width = size + 'px';
    g.style.height = size + 'px';
    g.style.left = (cx - wrapRect.left - half) + 'px';
    g.style.top  = (cy - wrapRect.top  - half) + 'px';

    g.style.backgroundImage = \"url('\" + img.src + \"')\";
    g.style.backgroundSize = (imgRect.width * zoom) + 'px ' + (imgRect.height * zoom) + 'px';

    const bgX = -(x * zoom - half);
    const bgY = -(y * zoom - half);
    g.style.backgroundPosition = bgX + 'px ' + bgY + 'px';
  }

  function onMouseMove(e){
    lastPos = { x: e.clientX, y: e.clientY };

    if (mouseDown){
      if (!dragStart) dragStart = { x: e.clientX, y: e.clientY };
      const dx = e.clientX - dragStart.x;
      const dy = e.clientY - dragStart.y;
      if (!dragging && (dx*dx + dy*dy) > 16){
        dragging = true;
        hideGlass();
        return;
      }
      if (dragging){
        hideGlass();
        return;
      }
    }
    updateGlassAt(e.clientX, e.clientY);
  }

  if (window.Shiny){
    Shiny.addCustomMessageHandler('magnifierSettings', function(msg){
      if (!msg) return;
      settings.enabled = !!msg.enabled;
      settings.zoom = Number(msg.zoom || settings.zoom);
      settings.size = Number(msg.size || settings.size);
      if (!settings.enabled) hideGlass();
      if (settings.enabled) scheduleReshow(0);
    });

    Shiny.addCustomMessageHandler('magnifierRefresh', function(msg){
      hideGlass();
      attachObserver();
      scheduleReshow(120);
    });
  }

  document.addEventListener('mousedown', function(e){
    mouseDown = true;
    dragStart = { x: e.clientX, y: e.clientY };
    dragging = false;
  });

  document.addEventListener('mouseup', function(){
    mouseDown = false;
    dragStart = null;
    if (dragging){
      dragging = false;
      scheduleReshow(80);
    } else {
      scheduleReshow(30);
    }
  });

  document.addEventListener('mousemove', onMouseMove);
  document.addEventListener('scroll', hideGlass, true);

  document.addEventListener('DOMContentLoaded', function(){
    attachObserver();
    setTimeout(attachObserver, 800);
  });
})();
"

read_png_rgb <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("png")) {
    arr <- png::readPNG(path)
  } else if (ext %in% c("jpg", "jpeg")) {
    arr <- jpeg::readJPEG(path)
  } else {
    stop("Unsupported image format: ", ext)
  }
  
  if (length(dim(arr)) == 2) {
    arr <- array(rep(arr, 3), dim = c(dim(arr), 3))
  }
  
  if (!is.null(dim(arr)) && length(dim(arr)) == 3 && dim(arr)[3] == 4) {
    arr <- arr[, , 1:3, drop = FALSE]
  }
  
  storage.mode(arr) <- "double"
  pmin(pmax(arr, 0), 1)
}

center_crop_rgb <- function(arr, target_h, target_w) {
  h <- dim(arr)[1]; w <- dim(arr)[2]
  if (h == target_h && w == target_w) return(arr)
  r0 <- floor((h - target_h) / 2) + 1
  c0 <- floor((w - target_w) / 2) + 1
  arr[r0:(r0 + target_h - 1), c0:(c0 + target_w - 1), , drop = FALSE]
}

compute_step <- function(h, w, max_dim) {
  max_dim <- as.integer(max_dim)
  if (!is.finite(max_dim) || max_dim < 100) max_dim <- 1200
  max(1L, as.integer(ceiling(max(h, w) / max_dim)))
}

luma_from_rgb <- function(r, g, b) 0.2989 * r + 0.5870 * g + 0.1140 * b

build_feature_df_all <- function(imgs) {
  use_modalities <- names(imgs)
  if (length(use_modalities) < 1) stop("No modalities available for features.")
  feat <- list()
  for (m in use_modalities) {
    a <- imgs[[m]]
    r <- as.vector(a[, , 1]); g <- as.vector(a[, , 2]); b <- as.vector(a[, , 3])
    feat[[paste0(m, "_r")]] <- r
    feat[[paste0(m, "_g")]] <- g
    feat[[paste0(m, "_b")]] <- b
    feat[[paste0(m, "_luma")]] <- luma_from_rgb(r, g, b)
  }
  as.data.frame(feat)
}

build_feature_df_at <- function(imgs, rows, cols) {
  use_modalities <- names(imgs)
  if (length(use_modalities) < 1) stop("No modalities available for features.")
  h <- dim(imgs[[use_modalities[1]]])[1]
  idx <- rows + (cols - 1) * h
  
  feat <- list()
  for (m in use_modalities) {
    a <- imgs[[m]]
    r <- as.vector(a[, , 1])[idx]
    g <- as.vector(a[, , 2])[idx]
    b <- as.vector(a[, , 3])[idx]
    feat[[paste0(m, "_r")]] <- r
    feat[[paste0(m, "_g")]] <- g
    feat[[paste0(m, "_b")]] <- b
    feat[[paste0(m, "_luma")]] <- luma_from_rgb(r, g, b)
  }
  as.data.frame(feat)
}

expand_clicks_to_patches <- function(click_df, h, w, radius = 1) {
  out_rows <- integer(0); out_cols <- integer(0); out_lab <- character(0)
  for (i in seq_len(nrow(click_df))) {
    r <- click_df$row[i]; c <- click_df$col[i]; lab <- click_df$label[i]
    rr <- seq(max(1, r - radius), min(h, r + radius))
    cc <- seq(max(1, c - radius), min(w, c + radius))
    grid <- expand.grid(row = rr, col = cc)
    out_rows <- c(out_rows, grid$row)
    out_cols <- c(out_cols, grid$col)
    out_lab  <- c(out_lab, rep(lab, nrow(grid)))
  }
  data.frame(row = out_rows, col = out_cols, label = out_lab, stringsAsFactors = FALSE)
}


clip01 <- function(x, eps = 1e-6) pmin(pmax(x, eps), 1 - eps)

compute_uncertainty <- function(p_mat, metric = c("entropy", "margin")) {
  metric <- match.arg(metric)
  p <- clip01(p_mat)
  if (metric == "entropy") {
    u <- -(p * log(p) + (1 - p) * log(1 - p)) / log(2)
    return(u)
  }
  4 * p * (1 - p)
}

scalar_to_rgb_grayscale <- function(m) {
  m <- pmin(pmax(m, 0), 1)
  h <- nrow(m); w <- ncol(m)
  rgb <- array(0, dim = c(h, w, 3))
  rgb[, , 1] <- m
  rgb[, , 2] <- m
  rgb[, , 3] <- m
  rgb
}

scalar_to_rgb_heat <- function(m) {
  m <- pmin(pmax(m, 0), 1)
  pal <- grDevices::colorRamp(c("#000000", "#FFD000", "#D40000"))
  cols <- pal(as.vector(m)) / 255
  h <- nrow(m); w <- ncol(m)
  rgb <- array(0, dim = c(h, w, 3))
  rgb[, , 1] <- matrix(cols[, 1], nrow = h, ncol = w)
  rgb[, , 2] <- matrix(cols[, 2], nrow = h, ncol = w)
  rgb[, , 3] <- matrix(cols[, 3], nrow = h, ncol = w)
  rgb
}

blend_rgb <- function(base_rgb, overlay_rgb, alpha = 0.5) {
  alpha <- pmin(pmax(alpha, 0), 1)
  out <- base_rgb * (1 - alpha) + overlay_rgb * alpha
  pmin(pmax(out, 0), 1)
}


make_disc_brush <- function(radius) {
  size <- 2 * radius + 1
  EBImage::makeBrush(size = size, shape = "disc")
}

mask_opening <- function(mask_hw, radius = 2) {
  m <- EBImage::Image(t(mask_hw) * 1, colormode = "Grayscale")
  if (radius > 0) m <- EBImage::opening(m, make_disc_brush(radius))
  t(EBImage::imageData(m) > 0.5)
}

label_pores <- function(mask_hw, separation_radius = 2, min_area_px = 10) {
  sep <- mask_opening(mask_hw, radius = separation_radius)
  lab_img <- EBImage::bwlabel(EBImage::Image(t(sep) * 1, colormode = "Grayscale"))
  lab_hw  <- t(EBImage::imageData(lab_img))
  
  ids <- lab_hw[lab_hw > 0]
  if (length(ids) == 0) return(list(labels = lab_hw, areas = numeric(0)))
  
  area_tab <- tabulate(as.integer(ids))
  keep <- which(area_tab >= min_area_px)
  if (length(keep) == 0) {
    lab_hw[,] <- 0
    return(list(labels = lab_hw, areas = numeric(0)))
  }
  
  new_lab <- matrix(0L, nrow = nrow(lab_hw), ncol = ncol(lab_hw))
  for (new_id in seq_along(keep)) {
    old_id <- keep[new_id]
    new_lab[lab_hw == old_id] <- as.integer(new_id)
  }
  list(labels = new_lab, areas = area_tab[keep])
}

compute_pore_features <- function(labels_hw, um_per_px = NA_real_) {
  n <- if (all(labels_hw == 0)) 0 else suppressWarnings(max(labels_hw, na.rm = TRUE))
  if (!is.finite(n) || n == 0) return(data.frame())
  
  ids <- labels_hw[labels_hw > 0]
  area <- tabulate(as.integer(ids), nbins = n)
  
  perim <- numeric(n)
  for (id in seq_len(n)) {
    m <- (labels_hw == id)
    if (!any(m)) next
    up    <- rbind(FALSE, m[-nrow(m), ])
    down  <- rbind(m[-1, ], FALSE)
    left  <- cbind(FALSE, m[, -ncol(m)])
    right <- cbind(m[, -1], FALSE)
    boundary <- m & !(up & down & left & right)
    perim[id] <- sum(boundary)
  }
  
  eq_diam <- sqrt(4 * area / pi)
  circularity <- ifelse(perim > 0, 4 * pi * area / (perim ^ 2), NA_real_)
  
  df <- data.frame(
    pore_id = seq_len(n),
    area_px = area,
    perimeter_px = perim,
    eq_diam_px = eq_diam,
    circularity = circularity,
    stringsAsFactors = FALSE
  )
  
  if (is.finite(um_per_px) && um_per_px > 0) {
    df$area_um2 <- df$area_px * (um_per_px ^ 2)
    df$perimeter_um <- df$perimeter_px * um_per_px
    df$eq_diam_um <- df$eq_diam_px * um_per_px
  } else {
    df$area_um2 <- NA_real_
    df$perimeter_um <- NA_real_
    df$eq_diam_um <- NA_real_
  }
  
  df
}

cluster_pores_kmeans <- function(pore_feat, k = 3) {
  if (nrow(pore_feat) == 0) return(pore_feat)
  k_req <- max(2, as.integer(k))
  
  use_phys <- all(c("area_um2", "eq_diam_um") %in% names(pore_feat)) &&
    any(is.finite(pore_feat$area_um2)) && any(is.finite(pore_feat$eq_diam_um))
  
  if (use_phys) {
    X <- pore_feat[, c("area_um2", "eq_diam_um", "circularity"), drop = FALSE]
    X$area_um2 <- log1p(X$area_um2)
  } else {
    X <- pore_feat[, c("area_px", "eq_diam_px", "circularity"), drop = FALSE]
    X$area_px <- log1p(X$area_px)
    names(X)[1] <- "area_um2"
    names(X)[2] <- "eq_diam_um"
  }
  
  for (j in seq_len(ncol(X))) {
    v <- X[[j]]
    if (any(!is.finite(v))) {
      med <- suppressWarnings(median(v[is.finite(v)], na.rm = TRUE))
      if (!is.finite(med)) med <- 0
      v[!is.finite(v)] <- med
      X[[j]] <- v
    }
  }
  
  X_mat <- as.matrix(X)
  n_unique <- nrow(unique(X_mat))
  attr(pore_feat, "kmeans_k_requested") <- k_req
  attr(pore_feat, "kmeans_distinct_points") <- n_unique
  
  if (!is.finite(n_unique) || n_unique < 2) {
    pore_feat$cluster <- "Cluster_1"
    attr(pore_feat, "kmeans_k_used") <- 1L
    attr(pore_feat, "kmeans_note") <- "K-means: not enough distinct data points; assigned all pores to Cluster_1."
    return(pore_feat)
  }
  
  k_used <- min(k_req, n_unique)
  attr(pore_feat, "kmeans_k_used") <- k_used
  note <- NULL
  if (k_used < k_req) {
    note <- sprintf("K-means: requested k=%d, but only %d distinct data points. Using k=%d to prevent a crash.",
                    k_req, n_unique, k_used)
  }
  
  X_scaled <- scale(X_mat)
  
  set.seed(1)
  km <- tryCatch(kmeans(X_scaled, centers = k_used, nstart = 10), error = function(e) e)
  if (inherits(km, "error")) {
    pore_feat$cluster <- "Cluster_1"
    attr(pore_feat, "kmeans_k_used") <- 1L
    attr(pore_feat, "kmeans_note") <- paste0("K-means failed; assigned all pores to Cluster_1. Error: ", km$message)
    return(pore_feat)
  }
  
  pore_feat$cluster <- paste0("Cluster_", km$cluster)
  attr(pore_feat, "kmeans_note") <- note
  pore_feat
}

make_cluster_summary <- function(pore_feat) {
  if (nrow(pore_feat) == 0) return(data.frame())
  
  df <- pore_feat
  df$count <- 1L
  
  has_um <- "area_um2" %in% names(df) && any(is.finite(df$area_um2))
  if (!has_um) df$area_um2 <- NA_real_
  
  agg <- aggregate(cbind(count, area_px, area_um2) ~ cluster, data = df, FUN = sum, na.rm = TRUE)
  
  total_n <- sum(agg$count)
  total_area <- if (has_um) sum(agg$area_um2) else sum(agg$area_px)
  
  agg$pct_count <- 100 * agg$count / total_n
  agg$pct_area  <- 100 * (if (has_um) agg$area_um2 else agg$area_px) / total_area
  
  agg[order(-agg$count), , drop = FALSE]
}

rep_pore_id_per_cluster <- function(pore_feat) {
  split_ids <- split(pore_feat, pore_feat$cluster)
  sapply(split_ids, function(df) {
    df <- df[order(df$area_px), , drop = FALSE]
    df$pore_id[ceiling(nrow(df) / 2)]
  }, simplify = TRUE, USE.NAMES = TRUE)
}

crop_rep_pore_images <- function(labels_hw, base_rgb, rep_ids_named, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  h <- nrow(labels_hw); w <- ncol(labels_hw)
  out_files <- character(0)
  
  for (g in names(rep_ids_named)) {
    id <- rep_ids_named[[g]]
    idx <- which(labels_hw == id, arr.ind = TRUE)
    if (nrow(idx) == 0) next
    
    rmin <- max(1, min(idx[, 1]) - 5)
    rmax <- min(h, max(idx[, 1]) + 5)
    cmin <- max(1, min(idx[, 2]) - 5)
    cmax <- min(w, max(idx[, 2]) + 5)
    
    crop <- base_rgb[rmin:rmax, cmin:cmax, , drop = FALSE]
    m    <- (labels_hw[rmin:rmax, cmin:cmax] == id)
    
    overlay <- crop
    overlay[, , 1] <- pmin(1, overlay[, , 1] + 0.7 * m)
    overlay[, , 2] <- overlay[, , 2] * (1 - 0.6 * m)
    overlay[, , 3] <- overlay[, , 3] * (1 - 0.6 * m)
    
    safe_g <- gsub("[^A-Za-z0-9_\\-]+", "_", g)
    f <- file.path(out_dir, paste0("rep_", safe_g, ".png"))
    png::writePNG(overlay, f)
    out_files <- c(out_files, f)
  }
  out_files
}


.okabe_ito7 <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
make_cluster_palette <- function(n) {
  if (n <= length(.okabe_ito7)) return(.okabe_ito7[seq_len(n)])
  grDevices::hcl.colors(n, palette = "Dark 3")
}

make_cluster_rgb <- function(labels_hw, pore_feat, cluster_levels, cluster_hex, background_hex = "#FFFFFF") {
  labels_hw <- matrix(as.integer(round(labels_hw)), nrow = nrow(labels_hw), ncol = ncol(labels_hw))
  h <- nrow(labels_hw); w <- ncol(labels_hw)
  
  if (is.null(pore_feat) || nrow(pore_feat) == 0) {
    rgb <- array(1, dim = c(h, w, 3))
    return(list(rgb = rgb, debug = list(labels_positive = sum(labels_hw > 0, na.rm = TRUE), colored_positive = 0L)))
  }
  
  pore_feat <- pore_feat[, c("pore_id", "cluster"), drop = FALSE]
  pore_feat$pore_id <- as.integer(pore_feat$pore_id)
  pore_feat$cluster <- as.character(pore_feat$cluster)
  
  cluster_levels <- as.character(cluster_levels)
  cluster_hex    <- as.character(cluster_hex)
  
  max_lab <- suppressWarnings(max(labels_hw, na.rm = TRUE))
  if (!is.finite(max_lab) || max_lab <= 0) {
    rgb <- array(1, dim = c(h, w, 3))
    return(list(rgb = rgb, debug = list(labels_positive = 0L, colored_positive = 0L)))
  }
  
  cidx <- match(pore_feat$cluster, cluster_levels)
  cidx[is.na(cidx)] <- 0L
  
  pore_to_cidx <- rep(0L, max_lab)
  ok <- !is.na(pore_feat$pore_id) & pore_feat$pore_id >= 1L & pore_feat$pore_id <= max_lab
  pore_to_cidx[pore_feat$pore_id[ok]] <- cidx[ok]
  
  cls_idx <- matrix(0L, nrow = h, ncol = w)
  inside <- labels_hw > 0
  cls_idx[inside] <- pore_to_cidx[labels_hw[inside]]
  cls_idx[is.na(cls_idx)] <- 0L
  
  colors <- c(background_hex, cluster_hex)
  rgb_lut <- t(grDevices::col2rgb(colors)) / 255
  
  idx <- cls_idx + 1L
  idx[idx < 1L] <- 1L
  idx[idx > nrow(rgb_lut)] <- 1L
  
  rgb <- array(0, dim = c(h, w, 3))
  rgb[, , 1] <- matrix(rgb_lut[idx, 1], nrow = h, ncol = w)
  rgb[, , 2] <- matrix(rgb_lut[idx, 2], nrow = h, ncol = w)
  rgb[, , 3] <- matrix(rgb_lut[idx, 3], nrow = h, ncol = w)
  rgb <- pmin(pmax(rgb, 0), 1)
  
  list(
    rgb = rgb,
    debug = list(
      labels_positive = sum(labels_hw > 0, na.rm = TRUE),
      colored_positive = sum(cls_idx > 0, na.rm = TRUE)
    )
  )
}

save_cluster_mask_with_legend <- function(cluster_rgb, legend_df, file,
                                          title = "Pore clusters",
                                          legend_width_px = 420) {
  cluster_rgb <- pmin(pmax(cluster_rgb, 0), 1)
  for (ch in 1:3) {
    channel <- cluster_rgb[, , ch]
    channel[is.na(channel)] <- 1
    cluster_rgb[, , ch] <- channel
  }
  
  h <- dim(cluster_rgb)[1]
  w <- dim(cluster_rgb)[2]
  w_total <- w + legend_width_px
  
  grDevices::png(filename = file, width = w_total, height = h)
  op <- par(no.readonly = TRUE)
  on.exit({ par(op); grDevices::dev.off() }, add = TRUE)
  
  par(mar = c(0,0,0,0))
  plot(NULL, xlim = c(0, w_total), ylim = c(0, h), axes = FALSE, xlab = "", ylab = "")
  rasterImage(as.raster(cluster_rgb), 0, 0, w, h)
  
  rect(w, 0, w_total, h, col = "white", border = NA)
  text(w + 20, h - 25, labels = title, adj = c(0, 0.5), cex = 1.1, font = 2)
  
  y <- h - 60
  dy <- 26
  for (i in seq_len(nrow(legend_df))) {
    if (y < 60) break
    col_hex <- legend_df$color_hex[i]
    if (is.na(col_hex) || !nzchar(col_hex)) col_hex <- "#999999"
    lab <- legend_df$label_line[i]
    if (is.na(lab) || !nzchar(lab)) lab <- "Unnamed"
    
    rect(w + 20, y - 10, w + 44, y + 10, col = col_hex, border = "grey30")
    text(w + 52, y, labels = lab, adj = c(0, 0.5), cex = 0.9)
    y <- y - dy
  }
}


ui <- fluidPage(
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(magnifier_js))
  ),
  
  div(class = "card",
      tags$div(style="font-size: 1.6rem; font-weight: 800;", "porositAI")
  ),
  
  uiOutput("status_ui"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      tabsetPanel(
        id = "side_tabs",
        type = "pills",
        
        tabPanel("Setup",
                 div(class="card",
                     tags$h4("Load images"),
                     fileInput("trans_file", "Translucent", accept = c(".png", ".jpg", ".jpeg")),
                     fileInput("polar_file", "Polarized", accept = c(".png", ".jpg", ".jpeg")),
                     fileInput("refrac_file", "Refracted", accept = c(".png", ".jpg", ".jpeg")),
                     
                     selectInput(
                       "display_image_type",
                       "Display image in viewer",
                       choices = c(translucent = "translucent", polarized = "polarized", refracted = "refracted"),
                       selected = "translucent"
                     )
                 ),
                 
                 div(class="card",
                     tags$h4("Working resolution (downsampling)"),
                     numericInput("working_max_dim", "Max dimension (pixels)", value = 1200, min = 400, max = 8000, step = 100),
                     tags$div(class="small-muted",
                              "This sets the working image size for speed vs accuracy. Aim for ~1–3 megapixels for smooth interaction; ~3–8 MP if your machine can handle it. Example: a 12 MP image (4000×3000) at max dim 1600 becomes ~1.9 MP."),
                     uiOutput("resolution_stats_ui")
                 ),
                 
                 div(class="card",
                     tags$h4("Image scale"),
                     numericInput("pixel_um", "Pixel size (µm per original pixel)", value = 5, min = 0.0001, step = 0.1),
                     tags$div(class="small-muted",
                              "Physical units are computed using pixel size × downsampling step. If you downsample, the app automatically scales the effective µm/pixel."),
                     uiOutput("pixel_scale_ui")
                 ),
                 
                 div(class="card",
                     tags$h4("Magnifier (loupe)"),
                     tags$div(class="small-muted",
                              "Hover to magnify around the cursor. Clicks keep the loupe visible; dragging (ROI brush) hides it."),
                     checkboxInput("magnifier_enabled", "Enable magnifier", value = TRUE),
                     sliderInput("magnifier_zoom", "Zoom", min = 2, max = 10, value = 4, step = 1),
                     sliderInput("magnifier_size", "Size (px)", min = 100, max = 260, value = 160, step = 10)
                 ),
                 
                 div(class="card",
                     tags$h4("ROI (optional)"),
                     tags$div(class="small-muted",
                              "Brush a rectangle on the image (main view), then click Set ROI."),
                     actionButton("set_roi", "Set ROI from brush"),
                     actionButton("clear_roi", "Clear ROI")
                 ),
                 
                 div(class="card",
                     tags$h4("Reset"),
                     tags$div(class="small-muted", "Reset analysis results while keeping loaded images."),
                     actionButton("reset_analysis", "Reset analysis (keep images)")
                 )
        ),
        
        tabPanel("Train",
                 div(class="card",
                     tags$h4("Training view"),
                     tags$div(class="small-muted", "Switch image types while adding training points."),
                     selectInput(
                       "display_image_type_train",
                       "Display image in viewer",
                       choices = c(translucent = "translucent", polarized = "polarized", refracted = "refracted"),
                       selected = "translucent"
                     )
                 ),
                 
                 div(class="card",
                     tags$h4("Click training"),
                     tags$div(class="small-muted", "Aim for ~20 clicks total: ~10 pores + ~10 solids. Spread them across the specimen and include confusers."),
                     radioButtons("label", "Current label:", choices = c("Pore", "Solid"), inline = TRUE),
                     checkboxInput("show_clicks", "Show clicks on image", value = TRUE),
                     sliderInput("click_size", "Click marker size", min = 0.6, max = 2.2, value = 1.1, step = 0.1),
                     sliderInput("patch_radius", "Auto-expand each click to a patch radius (px)", min = 0, max = 5, value = 1, step = 1),
                     uiOutput("click_stats_ui"),
                     actionButton("undo", "Undo last click"),
                     actionButton("clear_clicks", "Clear clicks")
                 ),
                 
                 div(class="card",
                     tags$h4("Magnifier (loupe)"),
                     tags$div(class="small-muted", "Mirrors Setup. Adjust while selecting points."),
                     checkboxInput("magnifier_enabled_train", "Enable magnifier", value = TRUE),
                     sliderInput("magnifier_zoom_train", "Zoom", min = 2, max = 10, value = 4, step = 1),
                     sliderInput("magnifier_size_train", "Size (px)", min = 100, max = 260, value = 160, step = 10)
                 ),
                 
                 div(class="card",
                     tags$h4("Train & segment"),
                     actionButton("train", "Train & segment preview"),
                     sliderInput("threshold", "Pore probability threshold", min = 0, max = 1, value = 0.50, step = 0.01),
                     uiOutput("porosity_ui")
                 ),
                 
                 div(class="card",
                     tags$h4("Uncertainty & active learning"),
                     tags$div(class="small-muted",
                              "After training, view uncertainty maps and generate suggested points. Add labels for those points and click Train again."),
                     selectInput("uncertainty_metric", "Uncertainty metric", choices = c(entropy = "entropy", margin = "margin"), selected = "entropy"),
                     sliderInput("uncertainty_alpha", "Uncertainty overlay opacity", min = 0, max = 1, value = 0.55, step = 0.05),
                     
                     actionButton("suggest_points", "Suggest uncertain points"),
                     sliderInput("suggest_top_pct", "Search top uncertain pixels (%)", min = 0.1, max = 5, value = 1, step = 0.1),
                     numericInput("n_suggest", "Number of suggested points", value = 25, min = 5, max = 200, step = 5),
                     numericInput("min_dist", "Min distance from existing clicks (px)", value = 12, min = 0, max = 200, step = 1),
                     
                     checkboxInput("show_suggestions", "Show suggested points on image", value = TRUE),
                     actionButton("clear_suggestions", "Clear suggested points"),
                     tableOutput("suggestions_table")
                 )
        ),
        
        tabPanel("Clustering",
                 div(class="card",
                     tags$h4("Separate & cluster pores"),
                     sliderInput("sep_radius", "Separation strength (opening radius, working px)", min = 0, max = 8, value = 2, step = 1),
                     numericInput("min_area", "Min pore area to keep (working px)", value = 10, min = 1, max = 100000, step = 1),
                     numericInput("k_clusters", "k (number of clusters)", value = 3, min = 2, max = 10, step = 1),
                     uiOutput("cluster_param_units_ui"),
                     actionButton("analyze_clusters", "Analyze clusters")
                 ),
                 div(class="card",
                     tags$h4("Rename clusters for exports"),
                     uiOutput("cluster_name_ui")
                 )
        ),
        
        tabPanel("Export",
                 div(class="card",
                     tags$h4("One-click export"),
                     tags$div(class="small-muted",
                              "Downloads a ZIP bundle with all available outputs (masks, overlays, tables, clustered masks, distributions, uncertainty maps, suggested points, settings)."),
                     downloadButton("dl_all", "Download ALL outputs (ZIP)")
                 ),
                 div(class="card",
                     tags$h4("Individual downloads"),
                     downloadButton("dl_mask", "pore_mask.png"),
                     downloadButton("dl_overlay", "overlay.png"),
                     downloadButton("dl_clicks", "clicks.csv"),
                     downloadButton("dl_pores", "pore_features.csv"),
                     downloadButton("dl_cluster_summary", "cluster_summary.csv"),
                     downloadButton("dl_cluster_mask", "pore_mask_clustered.png"),
                     downloadButton("dl_cluster_mask_legend", "pore_mask_clustered_legend.png"),
                     downloadButton("dl_cluster_bundle", "clustering_bundle.zip")
                 )
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        tabPanel("Annotate",
                 div(class="card",
                     tags$div(class="small-muted",
                              "Click to label pixels. Brush (drag) to define an ROI rectangle.")
                 ),
                 tags$div(
                   id = "img_plot_wrap",
                   style = "position:relative; width:100%;",
                   plotOutput(
                     "img_plot",
                     click = "img_click",
                     brush = brushOpts(id = "img_brush", resetOnNew = FALSE),
                     height = "650px"
                   ),
                   tags$div(id = "magnifier_glass")
                 )
        ),
        
        tabPanel("Segmentation",
                 div(class="card",
                     tags$div(class="small-muted", "Segmentation overlay preview (updates with threshold).")
                 ),
                 plotOutput("seg_plot", height = "650px")
        ),
        
        tabPanel("Uncertainty",
                 div(class="card",
                     tags$div(class="small-muted",
                              "Uncertainty map derived from classifier probabilities. High uncertainty suggests where extra training clicks can improve results.")
                 ),
                 plotOutput("uncert_plot", height = "650px")
        ),
        
        tabPanel("Clustering",
                 div(class="card",
                     tags$div(class="small-muted", "Clustered pore mask preview (color-coded by cluster).")
                 ),
                 plotOutput("cluster_mask_plot", height = "480px"),
                 
                 div(class="card",
                     tags$h4("Pore size distributions (equivalent diameter)"),
                     tags$div(class="small-muted", "Distributions are shown in µm when pixel size is provided; otherwise in pixels."),
                     plotOutput("cluster_dist_plot", height = "520px")
                 ),
                 
                 div(class="card",
                     tags$h4("Cluster summary"),
                     tableOutput("cluster_table")
                 ),
                 
                 div(class="card",
                     tags$h4("Representative pores"),
                     uiOutput("cluster_images_ui")
                 )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  out_dir <- file.path(tempdir(), "porositAI_out")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  addResourcePath("porositAI_out", out_dir)
  
  rv <- reactiveValues(
    imgs_full = NULL,   
    full_h = NULL, full_w = NULL,
    raw_dims = NULL,   
    imgs = NULL,       
    h = NULL, w = NULL,
    ds_step = 1L,
    
    clicks = data.frame(row = integer(), col = integer(), label = character(), stringsAsFactors = FALSE),
    roi = NULL,
    
    model = NULL,
    prob = NULL,
    mask = NULL,
    overlay = NULL,
    
    suggestions = NULL,
    
    pore_labels = NULL,
    pore_feat = NULL,
    cluster_summary = NULL,
    cluster_levels = NULL,
    rep_files = NULL
  )
  

  mag_state <- list(enabled = TRUE, zoom = 4, size = 160)
  syncing_mag <- reactiveVal(FALSE)
  
  observeEvent(list(input$magnifier_enabled, input$magnifier_zoom, input$magnifier_size), {
    if (isTRUE(syncing_mag())) return()
    syncing_mag(TRUE); on.exit(syncing_mag(FALSE), add = TRUE)
    if (!identical(input$magnifier_enabled_train, input$magnifier_enabled)) {
      updateCheckboxInput(session, "magnifier_enabled_train", value = isTRUE(input$magnifier_enabled))
    }
    if (!identical(input$magnifier_zoom_train, input$magnifier_zoom)) {
      updateSliderInput(session, "magnifier_zoom_train", value = input$magnifier_zoom)
    }
    if (!identical(input$magnifier_size_train, input$magnifier_size)) {
      updateSliderInput(session, "magnifier_size_train", value = input$magnifier_size)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$magnifier_enabled_train, input$magnifier_zoom_train, input$magnifier_size_train), {
    if (isTRUE(syncing_mag())) return()
    syncing_mag(TRUE); on.exit(syncing_mag(FALSE), add = TRUE)
    if (!identical(input$magnifier_enabled, input$magnifier_enabled_train)) {
      updateCheckboxInput(session, "magnifier_enabled", value = isTRUE(input$magnifier_enabled_train))
    }
    if (!identical(input$magnifier_zoom, input$magnifier_zoom_train)) {
      updateSliderInput(session, "magnifier_zoom", value = input$magnifier_zoom_train)
    }
    if (!identical(input$magnifier_size, input$magnifier_size_train)) {
      updateSliderInput(session, "magnifier_size", value = input$magnifier_size_train)
    }
  }, ignoreInit = TRUE)
  
  observe({
    mag_state <<- list(
      enabled = isTRUE(input$magnifier_enabled),
      zoom = as.numeric(input$magnifier_zoom %||% 4),
      size = as.numeric(input$magnifier_size %||% 160)
    )
    session$sendCustomMessage("magnifierSettings", mag_state)
  })
  
  send_magnifier_settings_cached <- function() {
    session$sendCustomMessage("magnifierSettings", mag_state)
  }
  

  syncing_disp <- reactiveVal(FALSE)
  
  observeEvent(input$display_image_type, {
    if (isTRUE(syncing_disp())) return()
    syncing_disp(TRUE); on.exit(syncing_disp(FALSE), add = TRUE)
    
    if (!identical(input$display_image_type_train, input$display_image_type)) {
      updateSelectInput(session, "display_image_type_train", selected = input$display_image_type)
    }
    
    session$onFlushed(function() {
      session$sendCustomMessage("magnifierRefresh", list())
      send_magnifier_settings_cached()
    }, once = TRUE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$display_image_type_train, {
    if (isTRUE(syncing_disp())) return()
    syncing_disp(TRUE); on.exit(syncing_disp(FALSE), add = TRUE)
    
    if (!identical(input$display_image_type, input$display_image_type_train)) {
      updateSelectInput(session, "display_image_type", selected = input$display_image_type_train)
    }
  }, ignoreInit = TRUE)
  

  reset_analysis_state <- function() {
    rv$clicks <- rv$clicks[0, , drop = FALSE]
    rv$roi <- NULL
    rv$model <- NULL
    rv$prob <- NULL
    rv$mask <- NULL
    rv$overlay <- NULL
    rv$suggestions <- NULL
    
    rv$pore_labels <- NULL
    rv$pore_feat <- NULL
    rv$cluster_summary <- NULL
    rv$cluster_levels <- NULL
    rv$rep_files <- NULL
  }
  

  pixel_um_original <- reactive({
    x <- suppressWarnings(as.numeric(input$pixel_um))
    if (!is.finite(x) || x <= 0) x <- 5
    x
  })
  
  um_per_px_working <- reactive({
    s <- rv$ds_step %||% 1L
    pixel_um_original() * as.numeric(s)
  })
  

  roi_mask <- reactive({
    req(rv$h, rv$w)
    h <- rv$h; w <- rv$w
    if (is.null(rv$roi)) return(matrix(TRUE, nrow = h, ncol = w))
    
    rmin <- max(1, min(h, min(rv$roi$rmin, rv$roi$rmax)))
    rmax <- max(1, min(h, max(rv$roi$rmin, rv$roi$rmax)))
    cmin <- max(1, min(w, min(rv$roi$cmin, rv$roi$cmax)))
    cmax <- max(1, min(w, max(rv$roi$cmin, rv$roi$cmax)))
    
    m <- matrix(FALSE, nrow = h, ncol = w)
    m[rmin:rmax, cmin:cmax] <- TRUE
    m
  })
  
  make_unique_names <- function(x) if (length(x) == 0) x else make.unique(x, sep = "_")
  

  rebuild_working_images <- function(max_dim, reset = TRUE) {
    req(rv$imgs_full, rv$full_h, rv$full_w)
    step <- compute_step(rv$full_h, rv$full_w, max_dim)
    rr <- seq(1, rv$full_h, by = step)
    cc <- seq(1, rv$full_w, by = step)
    
    imgs_ds <- lapply(rv$imgs_full, function(a) a[rr, cc, , drop = FALSE])
    
    rv$imgs <- imgs_ds
    rv$h <- dim(imgs_ds[[1]])[1]
    rv$w <- dim(imgs_ds[[1]])[2]
    rv$ds_step <- step
    
    choices_all <- c(translucent = "translucent", polarized = "polarized", refracted = "refracted")
    available <- intersect(names(choices_all), names(rv$imgs))
    if (length(available) == 0) available <- "translucent"
    selected <- if ("translucent" %in% available) "translucent" else available[1]
    
    updateSelectInput(session, "display_image_type", choices = choices_all[available], selected = selected)
    updateSelectInput(session, "display_image_type_train", choices = choices_all[available], selected = selected)
    
    if (reset) reset_analysis_state()
    
    session$onFlushed(function() {
      session$sendCustomMessage("magnifierRefresh", list())
      send_magnifier_settings_cached()
    }, once = TRUE)
  }
  

  observeEvent(list(input$trans_file, input$polar_file, input$refrac_file), {
    if (is.null(input$trans_file) && is.null(input$polar_file) && is.null(input$refrac_file)) return()
    
    imgs_raw <- list()
    if (!is.null(input$trans_file))  imgs_raw$translucent <- read_png_rgb(input$trans_file$datapath)
    if (!is.null(input$polar_file))  imgs_raw$polarized   <- read_png_rgb(input$polar_file$datapath)
    if (!is.null(input$refrac_file)) imgs_raw$refracted   <- read_png_rgb(input$refrac_file$datapath)
    
    rv$raw_dims <- lapply(imgs_raw, function(a) c(h = dim(a)[1], w = dim(a)[2]))
    
    hs <- sapply(imgs_raw, function(a) dim(a)[1])
    ws <- sapply(imgs_raw, function(a) dim(a)[2])
    target_h <- min(hs); target_w <- min(ws)
    
    imgs_crop <- lapply(imgs_raw, center_crop_rgb, target_h = target_h, target_w = target_w)
    
    rv$imgs_full <- imgs_crop
    rv$full_h <- target_h
    rv$full_w <- target_w
    
    rebuild_working_images(input$working_max_dim %||% 1200, reset = TRUE)
  })
  
  observeEvent(input$working_max_dim, {
    if (is.null(rv$imgs_full)) return()
    showNotification("Working resolution changed — resetting clicks/ROI/model (pixel coordinates changed).", type = "warning", duration = 6)
    rebuild_working_images(input$working_max_dim, reset = TRUE)
  }, ignoreInit = TRUE)
  

  output$resolution_stats_ui <- renderUI({
    if (is.null(rv$full_h) || is.null(rv$full_w) || is.null(rv$h) || is.null(rv$w)) {
      return(tags$div(class="small-muted", "Load images to see megapixel estimates."))
    }
    mp_full <- (rv$full_h * rv$full_w) / 1e6
    mp_work <- (rv$h * rv$w) / 1e6
    step <- rv$ds_step %||% 1L
    
    raw_txt <- ""
    if (!is.null(rv$raw_dims)) {
      parts <- sapply(names(rv$raw_dims), function(nm) {
        d <- rv$raw_dims[[nm]]
        sprintf("%s: %dx%d (%.2f MP)", nm, d["h"], d["w"], (d["h"]*d["w"])/1e6)
      })
      raw_txt <- paste(parts, collapse = " • ")
    }
    
    tags$div(
      class = "small-muted",
      tags$div(sprintf("Uploaded: %s", raw_txt)),
      tags$div(sprintf("Cropped analysis extent: %dx%d (%.2f MP)", rv$full_h, rv$full_w, mp_full)),
      tags$div(sprintf("Working extent: %dx%d (%.2f MP), downsample step = %d", rv$h, rv$w, mp_work, step))
    )
  })
  
  output$pixel_scale_ui <- renderUI({
    if (is.null(rv$ds_step)) {
      return(tags$div(class="small-muted", "Load images to compute effective µm/pixel at the working resolution."))
    }
    tags$div(
      class = "small-muted",
      sprintf("Effective working scale: %.4g µm / working pixel (pixel_um × step = %.4g × %d).",
              um_per_px_working(), pixel_um_original(), rv$ds_step %||% 1L)
    )
  })
  
  output$cluster_param_units_ui <- renderUI({
    if (is.null(rv$ds_step)) return(tags$div(class="small-muted", "Load images to see µm equivalents."))
    um_px <- um_per_px_working()
    sep_um <- (input$sep_radius %||% 0) * um_px
    min_area_um2 <- (input$min_area %||% 0) * (um_px^2)
    
    tags$div(
      class = "small-muted",
      sprintf("Separation radius ≈ %.3g µm. Min area ≈ %.3g µm² (based on working µm/pixel).", sep_um, min_area_um2)
    )
  })
  

  output$status_ui <- renderUI({
    imgs_ok   <- !is.null(rv$imgs)
    model_ok  <- !is.null(rv$prob)
    clusters_ok <- !is.null(rv$cluster_summary) && nrow(rv$cluster_summary) > 0
    roi_ok    <- !is.null(rv$roi)
    sugg_ok   <- !is.null(rv$suggestions) && nrow(rv$suggestions) > 0
    
    pore_n  <- sum(rv$clicks$label == "Pore", na.rm = TRUE)
    solid_n <- sum(rv$clicks$label == "Solid", na.rm = TRUE)
    clicks_ok <- (pore_n + solid_n) > 0
    
    div(class="card",
        tags$div(
          span(class = paste("badge", if (imgs_ok) "ok" else "off"),
               if (imgs_ok) "Images: loaded" else "Images: not loaded"),
          span(class = paste("badge", if (clicks_ok) "ok" else "off"),
               sprintf("Clicks: %d pore / %d solid", pore_n, solid_n)),
          span(class = paste("badge", if (model_ok) "ok" else "off"),
               if (model_ok) "Model: trained" else "Model: not trained"),
          span(class = paste("badge", if (sugg_ok) "ok" else "off"),
               if (sugg_ok) "Suggested points: ready" else "Suggested points: none"),
          span(class = paste("badge", if (clusters_ok) "ok" else "off"),
               if (clusters_ok) "Clustering: ready" else "Clustering: not ready"),
          span(class = paste("badge", if (roi_ok) "warn" else "off"),
               if (roi_ok) "ROI: ON" else "ROI: full image")
        )
    )
  })
  
  output$click_stats_ui <- renderUI({
    pore_n  <- sum(rv$clicks$label == "Pore", na.rm = TRUE)
    solid_n <- sum(rv$clicks$label == "Solid", na.rm = TRUE)
    total <- pore_n + solid_n
    div(class="small-muted",
        sprintf("Clicks so far: %d total (%d pore, %d solid).", total, pore_n, solid_n))
  })
  
  output$porosity_ui <- renderUI({
    if (is.null(rv$mask)) return(div(class="metric", "Porosity: (train model to compute)"))
    denom <- sum(roi_mask())
    por <- 100 * sum(rv$mask) / denom
    roi_note <- if (is.null(rv$roi)) "ROI: full image" else sprintf("ROI pixels: %d", denom)
    div(class="metric",
        HTML(sprintf("<b>Porosity:</b> %.2f%%<br/><span class='small-muted'>%s</span>", por, roi_note)))
  })
  

  observeEvent(input$reset_analysis, {
    reset_analysis_state()
    showNotification("Analysis reset (images kept).", type = "message")
  })
  

  observeEvent(input$img_click, {
    req(rv$imgs)
    x <- input$img_click$x
    y <- input$img_click$y
    if (is.null(x) || is.null(y)) return()
    
    h <- rv$h; w <- rv$w
    col <- floor(x) + 1
    row <- h - floor(y)
    
    col <- max(1, min(w, col))
    row <- max(1, min(h, row))
    
    rv$clicks <- rbind(rv$clicks,
                       data.frame(row = row, col = col, label = input$label, stringsAsFactors = FALSE))
  })
  
  observeEvent(input$undo, {
    if (nrow(rv$clicks) > 0) rv$clicks <- rv$clicks[-nrow(rv$clicks), , drop = FALSE]
  })
  
  observeEvent(input$clear_clicks, {
    rv$clicks <- rv$clicks[0, , drop = FALSE]
  })
  
  observeEvent(input$set_roi, {
    req(rv$imgs)
    b <- input$img_brush
    if (is.null(b)) {
      showNotification("No brush found. Drag a rectangle on the image first.", type = "warning")
      return()
    }
    
    h <- rv$h; w <- rv$w
    xmin <- min(b$xmin, b$xmax); xmax <- max(b$xmin, b$xmax)
    ymin <- min(b$ymin, b$ymax); ymax <- max(b$ymin, b$ymax)
    
    xmin <- max(0, min(w, xmin)); xmax <- max(0, min(w, xmax))
    ymin <- max(0, min(h, ymin)); ymax <- max(0, min(h, ymax))
    
    cmin <- max(1, min(w, floor(xmin) + 1))
    cmax <- max(1, min(w, ceiling(xmax)))
    
    rmin <- max(1, min(h, h - ceiling(ymax) + 1))
    rmax <- max(1, min(h, h - floor(ymin)))
    
    rv$roi <- list(rmin = rmin, rmax = rmax, cmin = cmin, cmax = cmax)
    showNotification("ROI set. Calculations now use ROI.", type = "message")
  })
  
  observeEvent(input$clear_roi, {
    rv$roi <- NULL
    showNotification("ROI cleared. Using full image.", type = "message")
  })
  

  observeEvent(input$train, {
    req(rv$imgs)
    if (nrow(rv$clicks) < 6) {
      showNotification("Add more clicks (recommend ~20 total).", type = "warning")
      return()
    }
    if (!("Pore" %in% rv$clicks$label) || !("Solid" %in% rv$clicks$label)) {
      showNotification("Need at least one Pore click and one Solid click.", type = "error")
      return()
    }
    
    rv$suggestions <- NULL
    
    withProgress(message = "Training pixel classifier", value = 0, {
      incProgress(0.10, detail = "Preparing training samples")
      h <- rv$h; w <- rv$w
      clicks_exp <- expand_clicks_to_patches(rv$clicks, h, w, radius = input$patch_radius)
      
      X_train <- build_feature_df_at(rv$imgs, rows = clicks_exp$row, cols = clicks_exp$col)
      y_train <- factor(tolower(clicks_exp$label), levels = c("solid", "pore"))
      train_df <- cbind(label = y_train, X_train)
      
      incProgress(0.45, detail = "Fitting model (random forest)")
      set.seed(1)
      rv$model <- ranger(label ~ ., data = train_df, probability = TRUE, num.trees = 300, min.node.size = 1)
      
      incProgress(0.80, detail = "Predicting pixels")
      X_all <- build_feature_df_all(rv$imgs)
      pred <- predict(rv$model, data = X_all)$predictions
      rv$prob <- matrix(pred[, "pore"], nrow = h, ncol = w)
      
      incProgress(1, detail = "Done")
    })
  })
  
  observe({
    req(rv$imgs)
    if (is.null(rv$prob)) return()
    
    base_mask <- rv$prob >= input$threshold
    rv$mask <- base_mask & roi_mask()
    
    base_mod <- if ("translucent" %in% names(rv$imgs)) "translucent" else names(rv$imgs)[1]
    base <- rv$imgs[[base_mod]]
    
    overlay <- base
    overlay[, , 1] <- pmin(1, overlay[, , 1] + 0.6 * rv$mask)
    overlay[, , 2] <- overlay[, , 2] * (1 - 0.5 * rv$mask)
    overlay[, , 3] <- overlay[, , 3] * (1 - 0.5 * rv$mask)
    rv$overlay <- overlay
  })
  

  uncertainty_mat <- reactive({
    req(rv$prob)
    u <- compute_uncertainty(rv$prob, metric = input$uncertainty_metric %||% "entropy")
    mroi <- roi_mask()
    u[!mroi] <- NA_real_
    u
  })
  

  observeEvent(input$clear_suggestions, {
    rv$suggestions <- NULL
  })
  
  observeEvent(input$suggest_points, {
    req(rv$prob, rv$imgs)
    u <- uncertainty_mat()
    if (all(!is.finite(u))) {
      showNotification("Uncertainty unavailable (train model first).", type = "warning")
      return()
    }
    
    top_pct <- as.numeric(input$suggest_top_pct %||% 1) / 100
    n_suggest <- as.integer(input$n_suggest %||% 25)
    min_dist <- as.numeric(input$min_dist %||% 0)
    
    u_vec <- as.vector(u)
    roi_vec <- as.vector(roi_mask())
    good <- roi_vec & is.finite(u_vec)
    vals <- u_vec[good]
    
    if (length(vals) < 50) {
      showNotification("Not enough valid ROI pixels to suggest points.", type = "warning")
      return()
    }
    
    thr <- as.numeric(stats::quantile(vals, probs = 1 - top_pct, na.rm = TRUE))
    cand <- which(roi_vec & is.finite(u_vec) & u_vec >= thr)
    if (length(cand) == 0) {
      showNotification("No candidate pixels found for the selected top %.", type = "warning")
      return()
    }
    
    if (length(cand) > 60000) cand <- sample(cand, 60000)
    
    ord <- cand[order(u_vec[cand], decreasing = TRUE)]
    ord <- ord[seq_len(min(length(ord), n_suggest * 50L))]
    
    h <- rv$h
    cand_row <- ((ord - 1) %% h) + 1
    cand_col <- ((ord - 1) %/% h) + 1
    
    ex <- rv$clicks
    ex_pts <- if (nrow(ex) > 0) cbind(ex$row, ex$col) else matrix(numeric(0), ncol = 2)
    
    sel_r <- integer(0); sel_c <- integer(0)
    
    for (i in seq_along(cand_row)) {
      r <- cand_row[i]; c <- cand_col[i]
      ok <- TRUE
      if (nrow(ex_pts) > 0 && min_dist > 0) {
        d2 <- (ex_pts[, 1] - r)^2 + (ex_pts[, 2] - c)^2
        if (min(d2) < (min_dist^2)) ok <- FALSE
      }
      if (length(sel_r) > 0 && min_dist > 0) {
        d2s <- (sel_r - r)^2 + (sel_c - c)^2
        if (min(d2s) < (min_dist^2)) ok <- FALSE
      }
      if (ok) {
        sel_r <- c(sel_r, r); sel_c <- c(sel_c, c)
        if (length(sel_r) >= n_suggest) break
      }
    }
    
    if (length(sel_r) == 0) {
      showNotification("Could not find suggested points given the distance constraint. Try lowering min distance or increasing top %.", type = "warning", duration = 8)
      return()
    }
    
    idx <- sel_r + (sel_c - 1) * h
    df <- data.frame(
      row = sel_r,
      col = sel_c,
      pore_prob = as.vector(rv$prob)[idx],
      uncertainty = as.vector(u)[idx],
      stringsAsFactors = FALSE
    )
    rv$suggestions <- df
    
    showNotification(sprintf("Suggested %d uncertain points. Label them (Pore/Solid), then click Train again.", nrow(df)), type = "message", duration = 8)
  })
  
  output$suggestions_table <- renderTable({
    if (is.null(rv$suggestions) || nrow(rv$suggestions) == 0) return(NULL)
    head(rv$suggestions, 25)
  }, digits = 3)
  

  output$img_plot <- renderPlot({
    req(rv$imgs)
    
    mod <- input$display_image_type %||% (if ("translucent" %in% names(rv$imgs)) "translucent" else names(rv$imgs)[1])
    base <- rv$imgs[[mod]]
    if (is.null(base)) base <- if ("translucent" %in% names(rv$imgs)) rv$imgs[["translucent"]] else rv$imgs[[1]]
    
    h <- dim(base)[1]; w <- dim(base)[2]
    par(mar = c(0, 0, 0, 0))
    plot(NULL, xlim = c(0, w), ylim = c(0, h), asp = 1, axes = FALSE, xlab = "", ylab = "")
    rasterImage(as.raster(base), 0, 0, w, h)
    
    if (!is.null(rv$roi)) {
      rmin <- min(rv$roi$rmin, rv$roi$rmax)
      rmax <- max(rv$roi$rmin, rv$roi$rmax)
      cmin <- min(rv$roi$cmin, rv$roi$cmax)
      cmax <- max(rv$roi$cmin, rv$roi$cmax)
      rect(xleft = cmin - 1, xright = cmax,
           ybottom = h - rmax, ytop = h - rmin + 1,
           border = "yellow", lwd = 2)
    }
    
    if (isTRUE(input$show_suggestions) && !is.null(rv$suggestions) && nrow(rv$suggestions) > 0) {
      x_s <- rv$suggestions$col - 0.5
      y_s <- h - rv$suggestions$row + 0.5
      points(x_s, y_s, pch = 1, cex = 1.5, lwd = 2, col = "orange")
    }
    
    if (isTRUE(input$show_clicks) && nrow(rv$clicks) > 0) {
      x_plot <- rv$clicks$col - 0.5
      y_plot <- h - rv$clicks$row + 0.5
      colp <- ifelse(rv$clicks$label == "Pore", "red", "cyan")
      points(x_plot, y_plot, pch = 16, cex = input$click_size, col = colp)
    }
    
    leg_items <- c()
    leg_pch <- c()
    leg_col <- c()
    if (isTRUE(input$show_clicks)) {
      leg_items <- c(leg_items, "Pore clicks", "Solid clicks")
      leg_pch <- c(leg_pch, 16, 16)
      leg_col <- c(leg_col, "red", "cyan")
    }
    if (isTRUE(input$show_suggestions) && !is.null(rv$suggestions) && nrow(rv$suggestions) > 0) {
      leg_items <- c(leg_items, "Suggested (uncertain)")
      leg_pch <- c(leg_pch, 1)
      leg_col <- c(leg_col, "orange")
    }
    if (length(leg_items) > 0) {
      legend("topright", inset = 0.01, legend = leg_items, pch = leg_pch, col = leg_col, bty = "n", cex = 0.9)
    }
  })
  
  output$seg_plot <- renderPlot({
    req(rv$imgs)
    if (is.null(rv$overlay)) {
      plot.new(); text(0.5, 0.5, "Train model to see segmentation.", cex = 1.2); return()
    }
    h <- rv$h; w <- rv$w
    par(mar = c(0, 0, 0, 0))
    plot(NULL, xlim = c(0, w), ylim = c(0, h), asp = 1, axes = FALSE, xlab = "", ylab = "")
    rasterImage(as.raster(rv$overlay), 0, 0, w, h)
  })
  
  output$uncert_plot <- renderPlot({
    req(rv$imgs)
    if (is.null(rv$prob)) {
      plot.new(); text(0.5, 0.5, "Train model to see uncertainty.", cex = 1.2); return()
    }
    
    base_mod <- if ("translucent" %in% names(rv$imgs)) "translucent" else names(rv$imgs)[1]
    base <- rv$imgs[[base_mod]]
    u <- uncertainty_mat()
    
    u2 <- u
    u2[!is.finite(u2)] <- 0
    heat <- scalar_to_rgb_heat(u2)
    
    alpha <- as.numeric(input$uncertainty_alpha %||% 0.55)
    overlay <- blend_rgb(base, heat, alpha = alpha)
    
    h <- rv$h; w <- rv$w
    par(mar = c(0,0,0,0))
    plot(NULL, xlim = c(0, w), ylim = c(0, h), asp = 1, axes = FALSE, xlab = "", ylab = "")
    rasterImage(as.raster(overlay), 0, 0, w, h)
  })
  

  observeEvent(input$analyze_clusters, {
    req(rv$mask)
    
    withProgress(message = "Separating & clustering pores", value = 0, {
      incProgress(0.2, detail = "Separating pores (opening + labeling)")
      lab_res <- label_pores(rv$mask, separation_radius = input$sep_radius, min_area_px = input$min_area)
      rv$pore_labels <- lab_res$labels
      
      incProgress(0.55, detail = "Computing pore features (px + µm)")
      feat <- compute_pore_features(rv$pore_labels, um_per_px = um_per_px_working())
      
      if (nrow(feat) == 0) {
        rv$pore_feat <- data.frame()
        rv$cluster_summary <- data.frame()
        rv$cluster_levels <- NULL
        rv$rep_files <- NULL
        showNotification("No pores found after separation/filtering. Try lowering min area or separation radius.", type = "warning")
        return()
      }
      
      incProgress(0.75, detail = "K-means clustering")
      feat <- cluster_pores_kmeans(feat, k = input$k_clusters)
      
      note <- attr(feat, "kmeans_note")
      k_used <- attr(feat, "kmeans_k_used")
      if (!is.null(note) && nzchar(note)) showNotification(note, type = "warning", duration = 10)
      if (!is.null(k_used) && is.finite(k_used) && k_used >= 1 && k_used != input$k_clusters) {
        updateNumericInput(session, "k_clusters", value = as.integer(k_used))
      }
      
      rv$pore_feat <- feat
      rv$cluster_summary <- make_cluster_summary(feat)
      rv$cluster_levels <- sort(unique(as.character(feat$cluster)))
      
      incProgress(0.90, detail = "Representative pore thumbnails")
      base_mod <- if ("translucent" %in% names(rv$imgs)) "translucent" else names(rv$imgs)[1]
      base <- rv$imgs[[base_mod]]
      rep_ids <- rep_pore_id_per_cluster(feat)
      rv$rep_files <- crop_rep_pore_images(rv$pore_labels, base, rep_ids, out_dir = out_dir)
      
      incProgress(1, detail = "Done")
    })
  })
  
  cluster_name_map <- reactive({
    if (is.null(rv$cluster_levels) || length(rv$cluster_levels) == 0) {
      return(setNames(character(0), character(0)))
    }
    new_names <- sapply(seq_along(rv$cluster_levels), function(i) {
      id <- paste0("cname_", i)
      val <- input[[id]]
      if (is.null(val) || !nzchar(val)) rv$cluster_levels[i] else val
    })
    stats::setNames(new_names, rv$cluster_levels)
  })
  
  output$cluster_name_ui <- renderUI({
    if (is.null(rv$cluster_levels) || length(rv$cluster_levels) == 0) {
      return(tags$div(class="small-muted", "Run 'Analyze clusters' to enable renaming."))
    }
    tagList(
      lapply(seq_along(rv$cluster_levels), function(i) {
        textInput(
          inputId = paste0("cname_", i),
          label = paste0("Cluster ", i, " (was: ", rv$cluster_levels[i], ")"),
          value = rv$cluster_levels[i]
        )
      })
    )
  })
  
  cluster_summary_named <- reactive({
    req(rv$cluster_summary)
    cs <- rv$cluster_summary
    if (nrow(cs) == 0) return(cs)
    
    nm <- cluster_name_map()
    if (length(nm) == 0) return(cs)
    
    disp <- unname(nm[rv$cluster_levels])
    disp[is.na(disp)] <- rv$cluster_levels[is.na(disp)]
    disp <- make_unique_names(disp)
    map_unique <- stats::setNames(disp, rv$cluster_levels)
    
    cs$cluster <- unname(map_unique[as.character(cs$cluster)])
    cs
  })
  
  output$cluster_table <- renderTable({
    cs <- cluster_summary_named()
    if (is.null(cs) || nrow(cs) == 0) return(NULL)
    cs
  }, digits = 3)
  
  output$cluster_mask_plot <- renderPlot({
    req(rv$imgs)
    if (is.null(rv$pore_labels) || is.null(rv$pore_feat) || is.null(rv$cluster_levels) || length(rv$cluster_levels) == 0) {
      plot.new(); text(0.5, 0.5, "Run 'Analyze clusters' to see clustered mask preview.", cex = 1.1); return()
    }
    
    pal <- make_cluster_palette(length(rv$cluster_levels))
    res <- make_cluster_rgb(rv$pore_labels, rv$pore_feat[, c("pore_id","cluster")], rv$cluster_levels, pal)
    
    rgb <- res$rgb
    h <- dim(rgb)[1]; w <- dim(rgb)[2]
    par(mar = c(0,0,0,0))
    plot(NULL, xlim = c(0, w), ylim = c(0, h), asp = 1, axes = FALSE, xlab = "", ylab = "")
    rasterImage(as.raster(rgb), 0, 0, w, h)
  })
  

  output$cluster_dist_plot <- renderPlot({
    req(rv$pore_feat)
    df <- rv$pore_feat
    if (nrow(df) == 0 || is.null(rv$cluster_levels) || length(rv$cluster_levels) == 0) {
      plot.new(); text(0.5, 0.5, "Run 'Analyze clusters' to see size distributions.", cex = 1.1); return()
    }
    
    nm <- cluster_name_map()
    disp_levels <- rv$cluster_levels
    if (length(nm) > 0) {
      disp2 <- unname(nm[rv$cluster_levels])
      disp2[is.na(disp2)] <- rv$cluster_levels[is.na(disp2)]
      disp_levels <- make_unique_names(disp2)
    }
    
    use_um <- "eq_diam_um" %in% names(df) && any(is.finite(df$eq_diam_um))
    xlab_global <- if (use_um) "Equivalent diameter (µm)" else "Equivalent diameter (px)"
    ylab_global <- "Number of pores"
    
    ng <- length(rv$cluster_levels)
    ncol <- ceiling(sqrt(ng))
    nrow <- ceiling(ng / ncol)
    n_panels <- nrow * ncol
    
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    
    par(mfrow = c(nrow, ncol), mar = c(2.5, 2.5, 2, 1), oma = c(4, 4, 0, 0))
    
    for (i in seq_len(n_panels)) {
      if (i <= ng) {
        cl <- rv$cluster_levels[i]
        label <- disp_levels[i]
        
        x <- if (use_um) df$eq_diam_um[df$cluster == cl] else df$eq_diam_px[df$cluster == cl]
        x <- x[is.finite(x)]
        
        if (length(x) < 2) {
          plot.new(); title(main = label); text(0.5, 0.5, "Not enough pores", cex = 0.9)
        } else {
          hist(x, breaks = "FD", main = label, xlab = "", ylab = "",
               col = "grey85", border = "white")
        }
      } else {
        plot.new()
      }
    }
    
    mtext(xlab_global, side = 1, outer = TRUE, line = 2)
    mtext(ylab_global, side = 2, outer = TRUE, line = 2)
  })
  
  output$cluster_images_ui <- renderUI({
    if (is.null(rv$rep_files) || length(rv$rep_files) == 0) {
      return(tags$div(class="small-muted", "Run 'Analyze clusters' to generate representative pore images."))
    }
    tags$div(lapply(rv$rep_files, function(f) {
      src <- paste0("porositAI_out/", basename(f))
      tags$div(style = "display:inline-block; margin:10px; text-align:center;",
               tags$img(src = src, width = "220px"),
               tags$div(class="small-muted", basename(f)))
    }))
  })
  
  
  output$dl_mask <- downloadHandler(
    filename = function() "pore_mask.png",
    content = function(file) { req(rv$mask); png::writePNG(rv$mask * 1, target = file) }
  )
  
  output$dl_overlay <- downloadHandler(
    filename = function() "overlay.png",
    content = function(file) { req(rv$overlay); png::writePNG(rv$overlay, target = file) }
  )
  
  output$dl_clicks <- downloadHandler(
    filename = function() "clicks.csv",
    content = function(file) { write.csv(rv$clicks, file, row.names = FALSE) }
  )
  
  output$dl_pores <- downloadHandler(
    filename = function() "pore_features.csv",
    content = function(file) {
      if (is.null(rv$pore_feat)) write.csv(data.frame(), file, row.names = FALSE)
      else write.csv(rv$pore_feat, file, row.names = FALSE)
    }
  )
  
  output$dl_cluster_summary <- downloadHandler(
    filename = function() "cluster_summary.csv",
    content = function(file) {
      cs <- cluster_summary_named()
      if (is.null(cs)) cs <- data.frame()
      write.csv(cs, file, row.names = FALSE)
    }
  )
  
  output$dl_cluster_mask <- downloadHandler(
    filename = function() "pore_mask_clustered.png",
    content = function(file) {
      req(rv$pore_labels, rv$pore_feat, rv$cluster_levels)
      pal <- make_cluster_palette(length(rv$cluster_levels))
      res <- make_cluster_rgb(rv$pore_labels, rv$pore_feat[, c("pore_id","cluster")], rv$cluster_levels, pal)
      png::writePNG(res$rgb, target = file)
    }
  )
  
  output$dl_cluster_mask_legend <- downloadHandler(
    filename = function() "pore_mask_clustered_legend.png",
    content = function(file) {
      req(rv$pore_labels, rv$pore_feat, rv$cluster_levels, rv$cluster_summary)
      
      pal <- make_cluster_palette(length(rv$cluster_levels))
      res <- make_cluster_rgb(rv$pore_labels, rv$pore_feat[, c("pore_id","cluster")], rv$cluster_levels, pal)
      
      nm <- cluster_name_map()
      cs <- rv$cluster_summary
      internal <- as.character(cs$cluster)
      
      display_names <- internal
      if (length(nm) > 0) {
        display_names <- unname(nm[internal])
        display_names[is.na(display_names)] <- internal[is.na(display_names)]
      }
      display_names <- make_unique_names(display_names)
      
      color_hex <- pal[match(internal, rv$cluster_levels)]
      color_hex[is.na(color_hex)] <- "#999999"
      
      label_line <- sprintf("%s   Area: %.1f%%   Count: %.1f%%",
                            display_names, cs$pct_area, cs$pct_count)
      
      legend_df <- data.frame(color_hex = color_hex, label_line = label_line, stringsAsFactors = FALSE)
      save_cluster_mask_with_legend(res$rgb, legend_df, file, title = "Pore clusters")
    }
  )
  
  output$dl_cluster_bundle <- downloadHandler(
    filename = function() "porositAI_clustering_bundle.zip",
    content = function(file) {
      req(rv$pore_labels, rv$pore_feat, rv$cluster_summary, rv$cluster_levels)
      
      tmp <- file.path(tempdir(), paste0("porositAI_cluster_bundle_", as.integer(Sys.time())))
      dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
      
      saveRDS(rv$pore_labels, file.path(tmp, "pore_labels.rds"))
      write.csv(rv$pore_feat, file.path(tmp, "pore_features.csv"), row.names = FALSE)
      write.csv(cluster_summary_named(), file.path(tmp, "cluster_summary.csv"), row.names = FALSE)
      
      nm <- cluster_name_map()
      if (length(nm) > 0) {
        nm_df <- data.frame(cluster = names(nm), display_name = unname(nm), stringsAsFactors = FALSE)
        write.csv(nm_df, file.path(tmp, "cluster_names.csv"), row.names = FALSE)
      }
      
      old <- setwd(tmp); on.exit(setwd(old), add = TRUE)
      files <- list.files(tmp, full.names = FALSE)
      utils::zip(zipfile = file, files = files)
    }
  )
  
  zip_dir <- function(zipfile, dir) {
    files <- list.files(dir, recursive = TRUE, full.names = FALSE)
    old <- setwd(dir); on.exit(setwd(old), add = TRUE)
    utils::zip(zipfile = zipfile, files = files)
  }
  
  save_cluster_distributions_png <- function(file) {
    req(rv$pore_feat, rv$cluster_levels)
    df <- rv$pore_feat
    ng <- length(rv$cluster_levels)
    if (ng < 1) return()
    
    nm <- cluster_name_map()
    disp_levels <- rv$cluster_levels
    if (length(nm) > 0) {
      disp2 <- unname(nm[rv$cluster_levels])
      disp2[is.na(disp2)] <- rv$cluster_levels[is.na(disp2)]
      disp_levels <- make_unique_names(disp2)
    }
    
    use_um <- "eq_diam_um" %in% names(df) && any(is.finite(df$eq_diam_um))
    xlab_global <- if (use_um) "Equivalent diameter (µm)" else "Equivalent diameter (px)"
    ylab_global <- "Number of pores"
    
    ncol <- ceiling(sqrt(ng))
    nrow <- ceiling(ng / ncol)
    n_panels <- nrow * ncol
    
    grDevices::png(file, width = 1200, height = 800)
    op <- par(no.readonly = TRUE)
    on.exit({ par(op); grDevices::dev.off() }, add = TRUE)
    
    par(mfrow = c(nrow, ncol), mar = c(2.5, 2.5, 2, 1), oma = c(4, 4, 0, 0))
    
    for (i in seq_len(n_panels)) {
      if (i <= ng) {
        cl <- rv$cluster_levels[i]
        label <- disp_levels[i]
        x <- if (use_um) df$eq_diam_um[df$cluster == cl] else df$eq_diam_px[df$cluster == cl]
        x <- x[is.finite(x)]
        if (length(x) < 2) {
          plot.new(); title(main = label); text(0.5, 0.5, "Not enough pores", cex = 0.9)
        } else {
          hist(x, breaks = "FD", main = label, xlab = "", ylab = "",
               col = "grey85", border = "white")
        }
      } else {
        plot.new()
      }
    }
    
    mtext(xlab_global, side = 1, outer = TRUE, line = 2)
    mtext(ylab_global, side = 2, outer = TRUE, line = 2)
  }
  
  output$dl_all <- downloadHandler(
    filename = function() paste0("porositAI_outputs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      tmp <- file.path(tempdir(), paste0("porositAI_all_", as.integer(Sys.time())))
      dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
      
      if (!is.null(rv$mask)) png::writePNG(rv$mask * 1, file.path(tmp, "pore_mask.png"))
      if (!is.null(rv$overlay)) png::writePNG(rv$overlay, file.path(tmp, "overlay.png"))
      if (!is.null(rv$clicks)) write.csv(rv$clicks, file.path(tmp, "clicks.csv"), row.names = FALSE)
      
      if (!is.null(rv$prob)) {
        prob_rgb <- scalar_to_rgb_grayscale(pmin(pmax(rv$prob, 0), 1))
        png::writePNG(prob_rgb, file.path(tmp, "pore_probability.png"))
        
        u <- compute_uncertainty(rv$prob, metric = input$uncertainty_metric %||% "entropy")
        u_rgb <- scalar_to_rgb_heat(pmin(pmax(u, 0), 1))
        png::writePNG(u_rgb, file.path(tmp, "uncertainty_map.png"))
      }
      
      if (!is.null(rv$suggestions)) write.csv(rv$suggestions, file.path(tmp, "suggested_points.csv"), row.names = FALSE)
      
      if (!is.null(rv$pore_feat)) write.csv(rv$pore_feat, file.path(tmp, "pore_features.csv"), row.names = FALSE)
      if (!is.null(rv$cluster_summary) && nrow(rv$cluster_summary) > 0) {
        write.csv(cluster_summary_named(), file.path(tmp, "cluster_summary.csv"), row.names = FALSE)
        
        pal <- make_cluster_palette(length(rv$cluster_levels))
        res <- make_cluster_rgb(rv$pore_labels, rv$pore_feat[, c("pore_id","cluster")], rv$cluster_levels, pal)
        png::writePNG(res$rgb, file.path(tmp, "pore_mask_clustered.png"))
        
        nm <- cluster_name_map()
        cs <- rv$cluster_summary
        internal <- as.character(cs$cluster)
        
        display_names <- internal
        if (length(nm) > 0) {
          display_names <- unname(nm[internal])
          display_names[is.na(display_names)] <- internal[is.na(display_names)]
        }
        display_names <- make_unique_names(display_names)
        
        color_hex <- pal[match(internal, rv$cluster_levels)]
        color_hex[is.na(color_hex)] <- "#999999"
        
        label_line <- sprintf("%s   Area: %.1f%%   Count: %.1f%%",
                              display_names, cs$pct_area, cs$pct_count)
        legend_df <- data.frame(color_hex = color_hex, label_line = label_line, stringsAsFactors = FALSE)
        save_cluster_mask_with_legend(res$rgb, legend_df, file.path(tmp, "pore_mask_clustered_legend.png"),
                                      title = "Pore clusters")
        
        save_cluster_distributions_png(file.path(tmp, "cluster_size_distributions.png"))
        
        if (!is.null(rv$rep_files) && length(rv$rep_files) > 0) {
          rep_dir <- file.path(tmp, "representative_pores")
          dir.create(rep_dir, recursive = TRUE, showWarnings = FALSE)
          file.copy(rv$rep_files, rep_dir, overwrite = TRUE)
        }
      }
      
      settings <- list(
        working_max_dim = input$working_max_dim,
        cropped_extent = c(h = rv$full_h, w = rv$full_w),
        working_extent = c(h = rv$h, w = rv$w),
        downsample_step = rv$ds_step,
        pixel_um_original = pixel_um_original(),
        pixel_um_working = um_per_px_working(),
        threshold = input$threshold,
        patch_radius = input$patch_radius,
        roi = rv$roi,
        separation_radius_px = input$sep_radius,
        min_area_px = input$min_area,
        k_clusters = input$k_clusters,
        uncertainty_metric = input$uncertainty_metric,
        suggest_top_pct = input$suggest_top_pct,
        n_suggest = input$n_suggest,
        min_dist = input$min_dist
      )
      writeLines(capture.output(str(settings)), file.path(tmp, "settings.txt"))
      writeLines(capture.output(sessionInfo()), file.path(tmp, "sessionInfo.txt"))
      
      writeLines(c(
        "porositAI output bundle",
        "",
        "Core files (if available):",
        "- pore_mask.png: binary pore mask (within ROI if set).",
        "- overlay.png: pore mask overlay on base image.",
        "- clicks.csv: clicked points used for training (row/col + label).",
        "- pore_probability.png: per-pixel P(pore).",
        "- uncertainty_map.png: uncertainty heatmap derived from P(pore).",
        "- suggested_points.csv: high-uncertainty points (if generated).",
        "",
        "Clustering files (if Analyze clusters was run):",
        "- pore_features.csv (includes px + µm units when pixel size provided).",
        "- cluster_summary.csv",
        "- pore_mask_clustered.png",
        "- pore_mask_clustered_legend.png",
        "- cluster_size_distributions.png",
        "- representative_pores/",
        "",
        "Repro:",
        "- settings.txt, sessionInfo.txt"
      ), file.path(tmp, "README_outputs.txt"))
      
      zip_dir(file, tmp)
    }
  )
}

shinyApp(ui, server)
