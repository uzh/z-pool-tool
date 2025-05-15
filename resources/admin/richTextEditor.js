import {
    Autoformat,
    BlockQuote,
    Bold,
    CKBox,
    CKFinder,
    CKFinderUploadAdapter,
    ClassicEditor,
    CloudServices,
    DomConverter,
    EasyImage,
    Essentials,
    Heading,
    Image,
    ImageCaption,
    ImageStyle,
    ImageToolbar,
    ImageUpload,
    Indent,
    Italic,
    Link,
    List,
    MediaEmbed,
    Paragraph,
    PasteFromOffice,
    PictureEditing,
    Table,
    TableToolbar,
    TextTransformation,
    viewToPlainText
} from 'ckeditor5';
import 'ckeditor5/dist/ckeditor5.css';

export class Editor extends ClassicEditor {
    static builtinPlugins = [
        Autoformat,
        BlockQuote,
        Bold,
        CKBox,
        CKFinder,
        CKFinderUploadAdapter,
        CloudServices,
        EasyImage,
        Essentials,
        Heading,
        Image,
        ImageCaption,
        ImageStyle,
        ImageToolbar,
        ImageUpload,
        Indent,
        Italic,
        Link,
        List,
        MediaEmbed,
        Paragraph,
        PasteFromOffice,
        PictureEditing,
        Table,
        TableToolbar,
        TextTransformation
    ];

    static defaultConfig = {
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
                'imageStyle:inline',
                'imageStyle:block',
                'imageStyle:side',
                '|',
                'toggleImageCaption',
                'imageTextAlternative'
            ]
        },
        table: {
            contentToolbar: [
                'tableColumn',
                'tableRow',
                'mergeTableCells'
            ]
        },
        language: 'en',
        licenseKey: 'GPL',
    };
}


export function initRichTextEditor(container = document) {
    container.querySelectorAll("textarea.rich-text").forEach(element => {
        Editor.create(element)
            .then((editor) => {
                editor.ui.view.element.classList.add('rich-text');
                const id = editor.sourceElement.id;
                const toggle = document.querySelector(`[data-toggle-reset-plaintext="${id}"]`)
                const plainText = document.querySelector(`[data-plain-text-for="${id}"]`)
                if (toggle && plainText) {
                    toggle.addEventListener("click", () => {
                        plainText.value = viewToPlainText(DomConverter, editor.editing.view.document.getRoot());
                    })
                }
            })
            .catch(error => {
                console.error(error);
            });
    })
}
