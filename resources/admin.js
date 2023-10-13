import { initButtonList } from "./admin/buttonList.js"
import { initCalendar } from './admin/calendar'
import { initCopyClipboard } from "./admin/copyClipboard.js";
import { initFilterForm } from "./admin/filter.js"
import { initPrint } from "./admin/print"
import { initRichTextEditor } from "./admin/richTextEditor.js"
import { initRoleSearchForm } from "./admin/role-search.js"

initButtonList()
initCalendar();
initCopyClipboard();
initFilterForm();
initPrint();
initRichTextEditor();
initRoleSearchForm();

window['pool-tool'] = {
    initRichTextEditor
};
