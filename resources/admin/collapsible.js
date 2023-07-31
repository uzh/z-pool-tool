
const activeClass = "active"

export const initCollapsible = () => {
    const lists = document.querySelectorAll(".collapsible-list")
    lists.forEach(wrapper => {
        const collapsibles = wrapper.querySelectorAll(".collapsible");
        collapsibles.forEach(el => {
            el.querySelector(".collapsible-header").addEventListener("click", () => {
                const isActive = el.classList.contains(activeClass)
                collapsibles.forEach(el => el.classList.remove(activeClass))
                if (!isActive) {
                    el.classList.add(activeClass)
                }
            })
        })
    })
}
