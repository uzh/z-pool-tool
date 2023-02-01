
import ClassicEditor from "@ckeditor/ckeditor5-build-classic";

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

export function initRichTextEditor() {
    document.querySelectorAll(".rich-text").forEach(element => {
        ClassicEditor.create(element)
            .catch(error => {
                console.error(error);
            });
    })
}
