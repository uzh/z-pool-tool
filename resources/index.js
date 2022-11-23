import 'htmx.org'
import './index.scss'
import { initDatepicker } from "./flatpickr.js"
import { initSortable } from "./sortable.js"
import framework from '../node_modules/@econ/frontend-framework/dist/main'
initDatepicker();
initSortable();

const activeClass = "active";

window.addEventListener("DOMContentLoaded", () => {
    document.querySelectorAll("[data-modal]").forEach((elm) => {
        const id = elm.dataset.modal;
        const target = document.getElementById(id);
        const body = document.getElementsByTagName("body")[0];
        const close = target.getElementsByClassName("modal-close");
        const toggleModal = () => {
            body.toggleAttribute("data-noscroll");
            target.classList.toggle(activeClass);
        }
        [...close, elm].forEach((elm) => elm.addEventListener("click", toggleModal))
        target.addEventListener("click", (e) => {
            const target = e.target;
            if (!target.classList.contains("modal-body") && !target.closest(".modal-body")) {
                toggleModal();
            }
        })
    });
});
