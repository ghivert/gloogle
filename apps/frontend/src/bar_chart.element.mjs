import { Chart } from 'chart.js/auto'

export class BarChart extends HTMLElement {
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
    const color = this.color
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
      type: 'bar',
      data: {
        labels,
        datasets: [
          {
            data,
            borderColor: `${color}aa`,
            backgroundColor: `${color}22`,
            borderRadius: 5,
          },
        ],
      },
      options: {
        aspectRatio: 1,
        indexAxis: 'y',
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
        elements: {
          bar: {
            borderWidth: 2,
            barPercentage: 0.5,
            barThickness: 6,
            maxBarThickness: 8,
            minBarLength: 2,
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
              mirror: true,
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
    customElements.define('bar-chart', BarChart)
  }
}
