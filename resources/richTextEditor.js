
import ClassicEditor from "@ckeditor/ckeditor5-build-classic";

export function initRichTextEditor() {
    document.querySelectorAll(".rich-text").forEach(element => {
        ClassicEditor.create(element)
            .catch(error => {
                console.error(error);
            });
    })
}
