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
    GeneralHtmlSupport,
    Heading,
    HeadingButtonsUI,
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
    ParagraphButtonUI,
    PasteFromOffice,
    PictureEditing,
    SourceEditing,
    Style,
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
        GeneralHtmlSupport,
        Heading,
        HeadingButtonsUI,
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
        ParagraphButtonUI,
        PasteFromOffice,
        PictureEditing,
        SourceEditing,
        Style,
        Table,
        TableToolbar,
        TextTransformation,
    ];

    static defaultConfig = {
        toolbar: {
            items: [
                'paragraph',
                'heading1',
                'heading2',
                'heading3',
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
                'sourceEditing',
                '|',
                'undo',
                'redo',
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
        heading: {
            options: [
                { model: 'paragraph', title: 'Paragraph', class: 'ck-heading_paragraph' },
                { model: 'heading1', view: 'h1', title: 'Heading 1', class: 'ck-heading_heading1' },
                { model: 'heading2', view: 'h2', title: 'Heading 2', class: 'ck-heading_heading2' },
                { model: 'heading3', view: 'h3', title: 'Heading 3', class: 'ck-heading_heading3' },
            ]
        },
        htmlSupport: {
            allow: [
                {
                    name: 'div',
                    classes: ['otp', 'apple-otp'],
                    attributes: true
                }
            ]
        }
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
