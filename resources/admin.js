import { initButtonList } from "./admin/buttonList.js"
import { initCalendar } from './admin/calendar'
import { initCopyClipboard } from "./admin/copyClipboard.js";
import { initFilterForm } from "./admin/filter.js"
import { initPrint } from "./admin/print"
import { initRichTextEditor } from "./admin/richTextEditor.js"
import { initHtmxSearch } from "./admin/search.js"

initButtonList()
initCalendar();
initCopyClipboard();
initFilterForm();
initPrint();
initRichTextEditor();
initHtmxSearch();

window['pool-tool'] = {
    initRichTextEditor
};

const initDuplicateSessionForm = () => {
    const form = document.getElementById("session-duplication-form")
    const subformWrapper = document.getElementById("session-duplication-subforms")

    if(form && subformWrapper) {
        form.addEventListener('htmx:configRequest', function(evt) {
            const counter = [...subformWrapper.querySelectorAll("[data-duplicate-form]")].reduce( (acc, el) => {
                const id = el.dataset.duplicateForm;
                return id && id > acc ? id : acc
            }, 0)
            evt.detail.parameters['counter'] = counter; 
        });
    }
}

initDuplicateSessionForm();