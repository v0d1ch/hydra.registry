import { useRef, useState, useCallback, useEffect } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import type { SubgraphNode, SubgraphEdge } from '../api/client'

interface Props {
  nodes: SubgraphNode[]
  edges: SubgraphEdge[]
}

interface PhysicsNode {
  id: string
  x: number
  y: number
  vx: number
  vy: number
  fx: number
  fy: number
  pinned: boolean
  node: SubgraphNode
}

const NODE_RADIUS = 3
const DAMPING = 0.85
const CENTER_GRAVITY = 0.01
// Reference size the physics were tuned for
const REF_SIZE = 800

// ─── Color palette ───

const PALETTE = [
  '#6e8cef', '#4db8a4', '#9b8ec4', '#c47a9a', '#d4a574',
  '#7ab5b0', '#8a9fd4', '#b09cc2', '#c9a96e', '#7c9eb8',
  '#a3b18a', '#c48b8b',
]

function nodeColor(i: number): string {
  return PALETTE[i % PALETTE.length]
}

// ─── Component ───

export default function RelayGraph({ nodes, edges }: Props) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)
  const physicsRef = useRef<PhysicsNode[]>([])
  const animRef = useRef<number>(0)
  const dragRef = useRef<{ nodeIdx: number; offsetX: number; offsetY: number } | null>(null)
  const dragStartedRef = useRef(false)
  const hoveredRef = useRef<number | null>(null)
  const [selected, setSelected] = useState<number | null>(null)
  const [, forceRender] = useState(0)
  const selectedRef = useRef<number | null>(null)
  selectedRef.current = selected

  // Adjacency data
  const adjRef = useRef<{ neighbors: Map<string, Set<string>>; edgeIndices: Map<string, Set<number>> }>({
    neighbors: new Map(), edgeIndices: new Map()
  })
  const nodeIndexRef = useRef<Map<string, number>>(new Map())

  // Initialize physics nodes when data changes
  useEffect(() => {
    // Place in normalized coords [0,1] — will be mapped to actual canvas in render loop
    const pNodes: PhysicsNode[] = nodes.map((node, i) => {
      const angle = (2 * Math.PI * i) / nodes.length
      const r = 0.35
      return {
        id: node.headId,
        x: 0.5 + r * Math.cos(angle) + (Math.random() - 0.5) * 0.03,
        y: 0.5 + r * Math.sin(angle) + (Math.random() - 0.5) * 0.03,
        vx: 0, vy: 0, fx: 0, fy: 0,
        pinned: false,
        node,
      }
    })
    physicsRef.current = pNodes

    // Build adjacency
    const adj = new Map<string, Set<string>>()
    const edgeSet = new Map<string, Set<number>>()
    edges.forEach((e, i) => {
      if (!adj.has(e.fromHead)) adj.set(e.fromHead, new Set())
      if (!adj.has(e.toHead)) adj.set(e.toHead, new Set())
      adj.get(e.fromHead)!.add(e.toHead)
      adj.get(e.toHead)!.add(e.fromHead)
      if (!edgeSet.has(e.fromHead)) edgeSet.set(e.fromHead, new Set())
      if (!edgeSet.has(e.toHead)) edgeSet.set(e.toHead, new Set())
      edgeSet.get(e.fromHead)!.add(i)
      edgeSet.get(e.toHead)!.add(i)
    })
    adjRef.current = { neighbors: adj, edgeIndices: edgeSet }

    const idxMap = new Map<string, number>()
    nodes.forEach((n, i) => idxMap.set(n.headId, i))
    nodeIndexRef.current = idxMap

    // When there's only one node (e.g. an isolated head looked up via
    // ?headId=…), open its detail panel automatically so the user sees the
    // same info they'd get by clicking it in a multi-node graph.
    setSelected(nodes.length === 1 ? 0 : null)
    hoveredRef.current = null
  }, [nodes, edges])

  // Physics + render loop — reads canvas display size each frame
  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return
    const ctx = canvas.getContext('2d')
    if (!ctx) return

    let running = true
    let lastW = 0
    let lastH = 0
    let initialized = false

    const tick = () => {
      if (!running) return
      const pNodes = physicsRef.current
      const n = pNodes.length
      if (n === 0) { animRef.current = requestAnimationFrame(tick); return }

      // Read actual display size and sync buffer
      const rect = canvas.getBoundingClientRect()
      const dpr = window.devicePixelRatio || 1
      const rw = Math.round(rect.width)
      const rh = Math.round(rect.height)
      if (rw < 10 || rh < 10) { animRef.current = requestAnimationFrame(tick); return }

      // First frame: map from normalized [0,1] coords to pixel coords
      if (!initialized) {
        for (let i = 0; i < n; i++) {
          pNodes[i].x *= rw
          pNodes[i].y *= rh
        }
        canvas.width = rw * dpr
        canvas.height = rh * dpr
        ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
        lastW = rw
        lastH = rh
        initialized = true
      } else if (Math.abs(rw - lastW) > 5 || Math.abs(rh - lastH) > 5) {
        // Subsequent resizes: scale positions proportionally
        const sx = rw / lastW
        const sy = rh / lastH
        const oldCx = lastW / 2
        const oldCy = lastH / 2
        const newCx = rw / 2
        const newCy = rh / 2
        for (let i = 0; i < n; i++) {
          pNodes[i].x = newCx + (pNodes[i].x - oldCx) * sx
          pNodes[i].y = newCy + (pNodes[i].y - oldCy) * sy
          pNodes[i].vx = 0
          pNodes[i].vy = 0
        }
        canvas.width = rw * dpr
        canvas.height = rh * dpr
        ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
        lastW = rw
        lastH = rh
      }
      const w = lastW
      const h = lastH

      const idxMap = nodeIndexRef.current
      const cx = w / 2
      const cy = h / 2
      const boundaryR = Math.min(w, h) * 0.45

      // Scale physics to canvas size so behavior is consistent across screen sizes
      const scale = Math.min(w, h) / REF_SIZE
      const repulsion = 800 * scale * scale
      const attraction = 0.005
      const idealLength = 120 * scale
      const maxSpeed = 10 * scale

      // Reset forces
      for (let i = 0; i < n; i++) { pNodes[i].fx = 0; pNodes[i].fy = 0 }

      // Repulsion
      for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
          const dx = pNodes[i].x - pNodes[j].x
          const dy = pNodes[i].y - pNodes[j].y
          const dist2 = dx * dx + dy * dy
          const dist = Math.sqrt(dist2) || 1
          const force = repulsion / dist2
          const fdx = (dx / dist) * force
          const fdy = (dy / dist) * force
          pNodes[i].fx += fdx; pNodes[i].fy += fdy
          pNodes[j].fx -= fdx; pNodes[j].fy -= fdy
        }
      }

      // Attraction (edges)
      for (const edge of edges) {
        const si = idxMap.get(edge.fromHead)
        const ti = idxMap.get(edge.toHead)
        if (si === undefined || ti === undefined) continue
        const dx = pNodes[ti].x - pNodes[si].x
        const dy = pNodes[ti].y - pNodes[si].y
        const dist = Math.sqrt(dx * dx + dy * dy) || 1
        const force = attraction * (dist - idealLength)
        const fdx = (dx / dist) * force
        const fdy = (dy / dist) * force
        pNodes[si].fx += fdx; pNodes[si].fy += fdy
        pNodes[ti].fx -= fdx; pNodes[ti].fy -= fdy
      }

      // Center gravity
      for (let i = 0; i < n; i++) {
        pNodes[i].fx += (cx - pNodes[i].x) * CENTER_GRAVITY
        pNodes[i].fy += (cy - pNodes[i].y) * CENTER_GRAVITY
      }

      // Apply forces + circular boundary
      for (let i = 0; i < n; i++) {
        if (pNodes[i].pinned) continue
        pNodes[i].vx = (pNodes[i].vx + pNodes[i].fx) * DAMPING
        pNodes[i].vy = (pNodes[i].vy + pNodes[i].fy) * DAMPING
        const speed = Math.sqrt(pNodes[i].vx ** 2 + pNodes[i].vy ** 2)
        if (speed > maxSpeed) {
          pNodes[i].vx = (pNodes[i].vx / speed) * maxSpeed
          pNodes[i].vy = (pNodes[i].vy / speed) * maxSpeed
        }
        pNodes[i].x += pNodes[i].vx
        pNodes[i].y += pNodes[i].vy

        // Circular boundary — push back toward center if outside radius
        const dx = pNodes[i].x - cx
        const dy = pNodes[i].y - cy
        const dist = Math.sqrt(dx * dx + dy * dy)
        if (dist > boundaryR) {
          const overshoot = dist - boundaryR
          pNodes[i].x -= (dx / dist) * overshoot * 0.5
          pNodes[i].y -= (dy / dist) * overshoot * 0.5
          // Dampen velocity toward boundary
          pNodes[i].vx *= 0.3
          pNodes[i].vy *= 0.3
        }
      }

      // ─── Render ───
      ctx.clearRect(0, 0, w, h)

      const hoverIdx = hoveredRef.current
      const selIdx = selectedRef.current
      const activeIdx = hoverIdx ?? selIdx
      const activeId = activeIdx !== null ? pNodes[activeIdx]?.id : null
      const activeNeighbors = activeId ? adjRef.current.neighbors.get(activeId) : null
      const activeEdges = activeId ? adjRef.current.edgeIndices.get(activeId) : null

      // Draw edges
      for (let i = 0; i < edges.length; i++) {
        const edge = edges[i]
        const si = idxMap.get(edge.fromHead)
        const ti = idxMap.get(edge.toHead)
        if (si === undefined || ti === undefined) continue

        const isActive = activeEdges?.has(i) ?? false
        const isDimmed = activeId !== null && !isActive

        ctx.beginPath()
        ctx.moveTo(pNodes[si].x, pNodes[si].y)
        ctx.lineTo(pNodes[ti].x, pNodes[ti].y)

        if (isActive) {
          const fromColor = nodeColor(si)
          ctx.strokeStyle = fromColor
          ctx.lineWidth = 1.5
          ctx.globalAlpha = 0.8
          ctx.shadowColor = fromColor
          ctx.shadowBlur = 6
        } else if (isDimmed) {
          ctx.strokeStyle = '#888'
          ctx.lineWidth = 0.3
          ctx.globalAlpha = 0.06
          ctx.shadowBlur = 0
        } else {
          ctx.strokeStyle = '#888'
          ctx.lineWidth = 0.5
          ctx.globalAlpha = 0.15
          ctx.shadowBlur = 0
        }
        ctx.stroke()
        ctx.shadowBlur = 0
        ctx.globalAlpha = 1
      }

      // Draw nodes
      for (let i = 0; i < n; i++) {
        const p = pNodes[i]
        const isActive = i === activeIdx
        const isNeighbor = activeNeighbors?.has(p.id) ?? false
        const isDimmed = activeId !== null && !isActive && !isNeighbor
        const isSel = i === selIdx
        const conns = adjRef.current.neighbors.get(p.id)?.size ?? 0
        // When only a single head is rendered (e.g. a focused isolated head
        // looked up via ?headId=…), bump the base radius so the dot is
        // actually visible on the canvas instead of a 3-pixel speck.
        const baseR = n === 1 ? 14 : NODE_RADIUS
        const r = baseR + Math.min(conns * 0.5, 4)
        const color = nodeColor(i)

        ctx.globalAlpha = isDimmed ? 0.1 : 1

        if (isActive || isSel) {
          ctx.beginPath()
          ctx.arc(p.x, p.y, r + 8, 0, Math.PI * 2)
          ctx.strokeStyle = color
          ctx.lineWidth = 1
          ctx.shadowColor = color
          ctx.shadowBlur = 12
          ctx.globalAlpha = isDimmed ? 0.1 : 0.5
          ctx.stroke()
          ctx.shadowBlur = 0
          ctx.globalAlpha = isDimmed ? 0.1 : 1
        }

        ctx.beginPath()
        ctx.arc(p.x, p.y, r, 0, Math.PI * 2)
        ctx.fillStyle = color
        ctx.globalAlpha = isDimmed ? 0.1 : (isActive || isSel ? 1 : 0.7)
        ctx.fill()

        if (isSel) {
          ctx.strokeStyle = '#fff'
          ctx.lineWidth = 1.5
          ctx.stroke()
        }

        if (p.node.hasHtlc) {
          ctx.beginPath()
          ctx.arc(p.x, p.y, Math.max(r - 2, 2), 0, Math.PI * 2)
          ctx.strokeStyle = '#fff'
          ctx.lineWidth = 0.5
          ctx.globalAlpha = isDimmed ? 0.05 : 0.6
          ctx.stroke()
        }

        ctx.globalAlpha = 1
      }

      // Hover tooltip
      if (hoverIdx !== null && selIdx === null && pNodes[hoverIdx]) {
        const p = pNodes[hoverIdx]
        const conns = adjRef.current.neighbors.get(p.id)?.size ?? 0
        const label = p.id.slice(0, 20) + '...'
        const meta = (p.node.hasHtlc ? 'HTLC  ' : '') + `${conns} connection${conns !== 1 ? 's' : ''}`
        const tw = 200
        const th = 44
        const flipX = p.x > w - tw - 20
        const flipY = p.y < th + 20
        const tx = flipX ? p.x - tw - 10 : p.x + 14
        const ty = flipY ? p.y + 14 : p.y - th - 6

        ctx.fillStyle = 'rgba(6, 6, 10, 0.9)'
        ctx.strokeStyle = 'rgba(77, 139, 255, 0.3)'
        ctx.lineWidth = 1
        roundRect(ctx, tx, ty, tw, th, 6)
        ctx.fill()
        ctx.stroke()
        ctx.fillStyle = '#4d8bff'
        ctx.font = '11px monospace'
        ctx.fillText(label, tx + 8, ty + 16)
        ctx.fillStyle = '#888'
        ctx.font = '10px sans-serif'
        ctx.fillText(meta, tx + 8, ty + 32)
      }

      animRef.current = requestAnimationFrame(tick)
    }

    animRef.current = requestAnimationFrame(tick)
    return () => { running = false; cancelAnimationFrame(animRef.current) }
  }, [edges, nodes])

  // ─── Pointer helpers ───

  const getNodeAt = useCallback((mx: number, my: number): number | null => {
    const pNodes = physicsRef.current
    for (let i = pNodes.length - 1; i >= 0; i--) {
      const dx = mx - pNodes[i].x
      const dy = my - pNodes[i].y
      const conns = adjRef.current.neighbors.get(pNodes[i].id)?.size ?? 0
      const baseR = pNodes.length === 1 ? 14 : NODE_RADIUS
      const r = baseR + Math.min(conns * 0.5, 4) + 6
      if (dx * dx + dy * dy < r * r) return i
    }
    return null
  }, [])

  const getPointerPos = useCallback((clientX: number, clientY: number): { x: number; y: number } => {
    const canvas = canvasRef.current
    if (!canvas) return { x: 0, y: 0 }
    const rect = canvas.getBoundingClientRect()
    return {
      x: (clientX - rect.left) * (canvas.width / (rect.width * (window.devicePixelRatio || 1))),
      y: (clientY - rect.top) * (canvas.height / (rect.height * (window.devicePixelRatio || 1))),
    }
  }, [])

  const handlePointerDown = useCallback((clientX: number, clientY: number) => {
    const pos = getPointerPos(clientX, clientY)
    const idx = getNodeAt(pos.x, pos.y)
    dragStartedRef.current = false
    if (idx !== null) {
      const p = physicsRef.current[idx]
      dragRef.current = { nodeIdx: idx, offsetX: pos.x - p.x, offsetY: pos.y - p.y }
      p.pinned = true
      p.vx = 0
      p.vy = 0
    }
  }, [getPointerPos, getNodeAt])

  const handlePointerMove = useCallback((clientX: number, clientY: number) => {
    const pos = getPointerPos(clientX, clientY)
    const drag = dragRef.current
    if (drag) {
      dragStartedRef.current = true
      const p = physicsRef.current[drag.nodeIdx]
      if (p) { p.x = pos.x - drag.offsetX; p.y = pos.y - drag.offsetY }
    } else {
      const idx = getNodeAt(pos.x, pos.y)
      hoveredRef.current = idx
      const canvas = canvasRef.current
      if (canvas) canvas.style.cursor = idx !== null ? 'grab' : 'default'
    }
  }, [getPointerPos, getNodeAt])

  const handlePointerUp = useCallback((clientX: number, clientY: number) => {
    const wasDragging = dragStartedRef.current
    const drag = dragRef.current
    if (drag) {
      physicsRef.current[drag.nodeIdx].pinned = false
      dragRef.current = null
    }
    if (!wasDragging) {
      const pos = getPointerPos(clientX, clientY)
      const idx = getNodeAt(pos.x, pos.y)
      if (idx !== null) {
        setSelected(prev => prev === idx ? null : idx)
        forceRender(v => v + 1)
      } else {
        setSelected(null)
        forceRender(v => v + 1)
      }
    }
  }, [getPointerPos, getNodeAt])

  const handlePointerLeave = useCallback(() => {
    hoveredRef.current = null
    const drag = dragRef.current
    if (drag) {
      physicsRef.current[drag.nodeIdx].pinned = false
      dragRef.current = null
    }
  }, [])

  // Mouse
  const onMouseDown = useCallback((e: React.MouseEvent) => handlePointerDown(e.clientX, e.clientY), [handlePointerDown])
  const onMouseMove = useCallback((e: React.MouseEvent) => handlePointerMove(e.clientX, e.clientY), [handlePointerMove])
  const onMouseUp = useCallback((e: React.MouseEvent) => handlePointerUp(e.clientX, e.clientY), [handlePointerUp])

  // Touch
  const onTouchStart = useCallback((e: React.TouchEvent) => {
    if (e.touches.length === 1) { e.preventDefault(); handlePointerDown(e.touches[0].clientX, e.touches[0].clientY) }
  }, [handlePointerDown])
  const onTouchMove = useCallback((e: React.TouchEvent) => {
    if (e.touches.length === 1) { e.preventDefault(); handlePointerMove(e.touches[0].clientX, e.touches[0].clientY) }
  }, [handlePointerMove])
  const onTouchEnd = useCallback((e: React.TouchEvent) => {
    if (e.changedTouches.length === 1) handlePointerUp(e.changedTouches[0].clientX, e.changedTouches[0].clientY)
  }, [handlePointerUp])

  // Selected node info
  const selectedNode = selected !== null ? physicsRef.current[selected]?.node ?? null : null
  const selectedConnections = selected !== null && selectedNode
    ? (adjRef.current.neighbors.get(selectedNode.headId)?.size ?? 0) : 0

  if (nodes.length === 0) {
    return (
      <div className="relay-graph-empty">
        No open heads with shared participants found on this network. This graph shows all open heads, not only those running HTLC.
      </div>
    )
  }

  return (
    <div className="relay-graph-container" ref={containerRef}>
      <canvas
        ref={canvasRef}
        className="relay-graph-canvas"
        onMouseDown={onMouseDown}
        onMouseMove={onMouseMove}
        onMouseUp={onMouseUp}
        onMouseLeave={handlePointerLeave}
        onTouchStart={onTouchStart}
        onTouchMove={onTouchMove}
        onTouchEnd={onTouchEnd}
      />

      <AnimatePresence>
        {selected !== null && selectedNode && (() => {
          const color = nodeColor(selected)
          return (
            <motion.div
              className="graph-detail-panel"
              initial={{ opacity: 0, y: 10 }}
              animate={{ opacity: 1, y: 0 }}
              exit={{ opacity: 0, y: 10 }}
              transition={{ duration: 0.2 }}
            >
              <div className="graph-detail-header">
                <span className="graph-detail-dot" style={{ background: color }} />
                <h4>Head Details</h4>
                <button className="graph-clear-btn" onClick={() => setSelected(null)}>Close</button>
              </div>
              <div className="graph-detail-body">
                <div className="graph-detail-row">
                  <span className="graph-detail-label">Head ID</span>
                  <code className="graph-detail-value">{selectedNode.headId}</code>
                </div>
                <div className="graph-detail-row">
                  <span className="graph-detail-label">Network</span>
                  <span className="graph-detail-value">{selectedNode.network}</span>
                </div>
                <div className="graph-detail-row">
                  <span className="graph-detail-label">TVL</span>
                  <span className="graph-detail-value">{(selectedNode.committedLovelace / 1_000_000).toFixed(2)} ADA</span>
                </div>
                <div className="graph-detail-row">
                  <span className="graph-detail-label">HTLC</span>
                  <span className="graph-detail-value">{selectedNode.hasHtlc ? 'Enabled' : 'No'}</span>
                </div>
                <div className="graph-detail-row">
                  <span className="graph-detail-label">Connections</span>
                  <span className="graph-detail-value">{selectedConnections} head{selectedConnections !== 1 ? 's' : ''}</span>
                </div>
                <div className="graph-detail-row graph-detail-row-full">
                  <span className="graph-detail-label">Participants ({selectedNode.participants.length})</span>
                  <div className="graph-detail-participants">
                    {selectedNode.participants.map((addr, i) => (
                      <code key={i} className="graph-participant-addr">{addr}</code>
                    ))}
                  </div>
                </div>
              </div>
            </motion.div>
          )
        })()}
      </AnimatePresence>

      <div className="relay-graph-legend">
        <span className="legend-item">
          <svg width="10" height="10"><circle cx="5" cy="5" r="4" fill="#6e8cef" /></svg>
          Head
        </span>
        <span className="legend-item">
          <svg width="10" height="10"><circle cx="5" cy="5" r="4" fill="#6e8cef" stroke="#fff" strokeWidth="1" /></svg>
          HTLC
        </span>
        <span className="legend-item">
          <svg width="16" height="6"><line x1="0" y1="3" x2="16" y2="3" stroke="#6e8cef" strokeWidth="1.5" /></svg>
          Connection
        </span>
        <span className="legend-item relay-graph-count">{nodes.length} heads, {edges.length} links</span>
      </div>
    </div>
  )
}

function roundRect(ctx: CanvasRenderingContext2D, x: number, y: number, w: number, h: number, r: number) {
  ctx.beginPath()
  ctx.moveTo(x + r, y)
  ctx.lineTo(x + w - r, y)
  ctx.quadraticCurveTo(x + w, y, x + w, y + r)
  ctx.lineTo(x + w, y + h - r)
  ctx.quadraticCurveTo(x + w, y + h, x + w - r, y + h)
  ctx.lineTo(x + r, y + h)
  ctx.quadraticCurveTo(x, y + h, x, y + h - r)
  ctx.lineTo(x, y + r)
  ctx.quadraticCurveTo(x, y, x + r, y)
  ctx.closePath()
}
