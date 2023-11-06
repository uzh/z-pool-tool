import 'htmx.org'
import './index.scss'
import { initDatepicker } from "./flatpickr.js"
import { initHTMX } from './htmx'
import { initSearch } from "./search"
import {
    initConfirmable,
    initCollapsible,
    initFileInput,
    initModal,
    initNotification,
    initSortable,
    initDetectFormChanges,
} from '../node_modules/@econ/frontend-framework/dist/main'
import { initFormSubmitInterferer } from "./formSubmitInterferer.js"

initDatepicker();
initConfirmable();
initCollapsible();
initFileInput();
initModal();
initNotification();
initSortable();
initDetectFormChanges();
initFormSubmitInterferer();
initHTMX();
initSearch();
