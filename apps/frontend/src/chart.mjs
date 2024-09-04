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
    // wrapper.style.maxWidth = '400px'
    // wrapper.style.maxHeight = '150px'
    this.#canvas = document.createElement('canvas')
    wrapper.appendChild(this.#canvas)
    this.#shadow.appendChild(wrapper)

    new Chart(this.#canvas, {
      type: 'line',
      data: {
        labels,
        datasets: [
          {
            data,
            borderColor: '#8c3a96',
            fill: false,
            tension: 0.4,
          },
        ],
      },
      options: {
        responsive: true,
        animation: false,
        events: [],
        plugins: {
          legend: {
            display: false,
          },
        },
        scales: {
          x: { display: true, title: { display: true } },
          y: { display: true, title: { display: true, text: 'Value' } },
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
