import ClassicEditor from "@ckeditor/ckeditor5-build-classic";
import viewToPlainText from '@ckeditor/ckeditor5-clipboard/src/utils/viewtoplaintext';

ClassicEditor.defaultConfig = {
    toolbar: {
        items: [
            'heading',
            '|',
            'bold',
            'italic',
            'link',
            '|',
            'bulletedList',
            'numberedList',
            '|',
            'outdent',
            'indent',
            '|',
            'insertTable',
            'undo',
            'redo'
        ]
    },
    image: {
        toolbar: [
            'imageStyle:full',
            'imageStyle:side',
            '|',
            'imageTextAlternative'
        ]
    },
    table: {
        contentToolbar: ['tableColumn', 'tableRow', 'mergeTableCells']
    },
    language: 'en'
};

export function initRichTextEditor(container = document) {
    container.querySelectorAll(".rich-text").forEach(element => {
        ClassicEditor.create(element)
            .then((editor) => {
                const id = editor.sourceElement.id;
                const toggle = document.querySelector(`[data-toggle-reset-plaintext="${id}"]`)
                const plainText = document.querySelector(`[data-plain-text-for="${id}"]`)
                if (toggle && plainText) {
                    toggle.addEventListener("click", () => {
                        plainText.value = viewToPlainText(editor.editing.view.document.getRoot());
                    })
                }
            })
            .catch(error => {
                console.error(error);
            });
    })
}
