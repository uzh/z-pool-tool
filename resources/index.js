import 'htmx.org'
import './index.scss'
import { initNavitation } from './navigation.js'
import { initDatepicker } from "./flatpickr.js"
import { initHTMX } from './htmx'
import { initSearch } from "./search"
import {
    initConfirmable,
    initCollapsible,
    initDetectDropdownPositions,
    initFileInput,
    initModal,
    initNotification,
    initSortable,
    initDetectFormChanges,
} from '../node_modules/@econ/frontend-framework/dist/main'
import { initFormSubmitInterferer } from "./formSubmitInterferer.js"
import { initRichTextEditor } from "./admin/richTextEditor.js"

initNavitation();
initDatepicker();
initConfirmable();
initCollapsible();
initDetectDropdownPositions();
initFileInput();
initModal();
initNotification();
initSortable();
initDetectFormChanges();
initFormSubmitInterferer();
initHTMX();
initRichTextEditor();
initSearch();
