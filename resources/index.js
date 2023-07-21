import 'htmx.org'
import './index.scss'
import { initDatepicker } from "./flatpickr.js"
import { initCalendar } from './calendar'
import {
    initConfirmable,
    initFileInput,
    initModal,
    initNotification,
    initSortable,
    initDetectFormChanges,
} from '../node_modules/@econ/frontend-framework/dist/main'

initDatepicker();
initConfirmable();
initFileInput();
initModal();
initNotification();
initSortable();
initDetectFormChanges();
initCalendar();
