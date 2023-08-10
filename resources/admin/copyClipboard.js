export const initCopyClipboard = () => {
    document.querySelectorAll("[data-clipboard]").forEach((el) => {
        el.addEventListener("click", (ev) => {
            const { clipboard } = ev.currentTarget.dataset
            if (clipboard) {
                navigator.clipboard.writeText(clipboard)
            }
        })
    })
}
