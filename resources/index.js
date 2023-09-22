import 'htmx.org'
import './index.scss'
import { initDatepicker } from "./flatpickr.js"
import {
    initConfirmable,
    initFileInput,
    initModal,
    initNotification,
    initSortable,
    initDetectFormChanges,
} from '../node_modules/@econ/frontend-framework/dist/main'
import { initFormSubmitInterferer } from "./formSubmitInterferer.js"

initDatepicker();
initConfirmable();
initFileInput();
initModal();
initNotification();
initSortable();
initDetectFormChanges();
initFormSubmitInterferer();
