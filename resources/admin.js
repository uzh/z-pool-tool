import { initRoleSearchForm } from "./admin/role-search.js"
import { initFilterForm } from "./admin/filter.js"
import { initRichTextEditor } from "./admin/richTextEditor.js"
import { initCopyClipboard } from "./admin/copyClipboard.js";
import { initCalendar } from './admin/calendar'

initRoleSearchForm();
initFilterForm();
initRichTextEditor();
initCopyClipboard();
initCalendar();

window['pool-tool'] = {
    initRichTextEditor
};
