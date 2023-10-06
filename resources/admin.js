import { initRoleSearchForm } from "./admin/role-search.js"
import { initFilterForm } from "./admin/filter.js"
import { initRichTextEditor } from "./admin/richTextEditor.js"
import { initCopyClipboard } from "./admin/copyClipboard.js";
import { initCalendar } from './admin/calendar'
import { initPrint } from "./admin/print"

initRoleSearchForm();
initFilterForm();
initRichTextEditor();
initCopyClipboard();
initCalendar();
initPrint();

window['pool-tool'] = {
    initRichTextEditor
};
