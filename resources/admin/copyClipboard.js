export const initCopyClipboard = () => {
    document.addEventListener("click", (ev) => {
        const { clipboard } = ev.target.closest("[data-clipboard]")?.dataset ?? {}
        if (clipboard) {
            navigator.clipboard.writeText(clipboard)
        }
    })
}
