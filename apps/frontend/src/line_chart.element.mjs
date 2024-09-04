import { Chart } from 'chart.js/auto'

export class LineChart extends HTMLElement {
  static observedAttributes = ['datasets']

  #shadow
  #canvas
  dataset

  constructor() {
    super()
    this.#shadow = this.attachShadow({ mode: 'open' })
  }

  connectedCallback() {
    this.#render()
  }

  #render() {
    const labels = this.datasets.labels.toArray()
    const data = this.datasets.data.toArray()
    const wrapper = document.createElement('div')
    wrapper.style.position = 'relative'
    wrapper.style.maxWidth = '850px'
    // wrapper.style.padding = '12px'
    // wrapper.style.maxHeight = '150px'
    this.#canvas = document.createElement('canvas')
    wrapper.appendChild(this.#canvas)
    this.#shadow.appendChild(wrapper)
    Chart.defaults.font.family = 'Lexend'
    new Chart(this.#canvas, {
      type: 'line',
      data: {
        labels,
        datasets: [
          {
            data,
            borderColor: '#8c3a96aa',
            backgroundColor: '#8c3a9622',
            fill: true,
            tension: 0.4,
          },
        ],
      },
      options: {
        responsive: true,
        animation: false,
        events: [],
        layout: {
          padding: {
            right: 12,
          },
        },
        plugins: {
          legend: {
            display: false,
          },
        },
        scales: {
          x: {
            display: true,
            grid: { drawTicks: false, display: true },
            ticks: { padding: 0, align: 'inner', padding: 5 },
          },
          y: {
            display: true,
            grid: { drawTicks: false, display: true },
            ticks: {
              padding: 5,
              // mirror: true,
              includeBounds: false,
              backdropPadding: 0,
            },
          },
        },
      },
    })
  }

  // Lifecycle functions.
  disconnectedCallback() {}
  adoptedCallback() {}

  attributeChangedCallback() {}

  static register() {
    customElements.define('line-chart', LineChart)
  }
}
