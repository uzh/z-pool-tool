const createTag = (option, name, inputType) => {
    const item = document.createElement("span");
    item.setAttribute("data-selection-item", "");
    item.innerHTML = option.innerHTML;
    const icon = document.createElement("i");
    icon.classList.add("icon-close");
    const input = document.createElement("input");
    input.setAttribute("type", "checkbox");
    if (inputType) {
        input.setAttribute("data-input-type", "option")
    }
    input.checked = true;
    input.setAttribute("hidden", "")
    input.value = option.dataset.value;
    input.name = name;
    item.appendChild(icon);
    item.appendChild(input);
    return item;
}

const addRemoveItemListener = (item, callback) => {
    item.querySelector(".icon-close").addEventListener("click", () => {
        item.remove()
        if (callback) {
            callback();
        }
    }, { once: true })
}

class Search {
    constructor(input) {
        const { name, inputType } = input.dataset;
        this.input = input;
        this.name = name;
        this.inputType = inputType;
        this.wrapper = this.input.closest(".form-group");
        this.selection = this.wrapper.querySelector("[data-search-selection]");
        this.optionsWrapper = this.wrapper.querySelector(".data-list");

        this.addOptionsCloseListener();
    }

    addSelectedItemsRemoveListeners(callback) {
        [...this.selection.querySelectorAll("[data-selection-item]")].forEach(item => addRemoveItemListener(item, callback))
    }

    addOptionsCloseListener() {
        document.addEventListener("click", (e) => {
            if (!this.wrapper.contains(e.target)) {
                this.optionsWrapper.classList.remove("active");
            }
        });
    }

    createSelectedItem(option) {
        const item = createTag(option, this.name, this.inputType);
        this.selection.appendChild(item);
        addRemoveItemListener(item);
    }
}

class StaticSearch extends Search {
    constructor(input) {
        super(input);
        this.options = this.optionsWrapper.querySelectorAll(".data-item");
        this.filterOptions();
        this.addShowOptionsEventListeners();
        this.addInputEventListener();
        this.addClickOptionListener();
        this.addSelectedItemsRemoveListeners(() => this.filterOptions());
    }

    filterOptions() {
        const value = this.input.value.toLocaleLowerCase();
        const selected = [...this.selection.querySelectorAll("[data-selection-item] input")].map((item) => item.value)
        this.options.forEach((option) => {
            if (option.innerText.toLocaleLowerCase().includes(value) && !selected.includes(option.dataset.value)) {
                option.style.display = "flex";
            } else {
                option.style.display = "none";
            }
        })
    }

    addShowOptionsEventListeners() {
        this.input.addEventListener("focusin", () => {
            this.optionsWrapper.classList.add("active");
        })
    }

    addInputEventListener() {
        this.input.addEventListener("keyup", () => {
            this.filterOptions();
        });
    }

    addClickOptionListener() {
        [...this.options].forEach(option => {
            option.addEventListener("click", () => {
                this.createSelectedItem(option);
                option.style.display = "none";
            })
        })
    }
}

class DynamicSearch extends Search {
    constructor(input) {
        super(input);
        this.addClickOptionListener();
        this.addSelectedItemsRemoveListeners();
    }

    addClickOptionListener() {
        this.wrapper.addEventListener("htmx:afterSwap", (e) => {
            [...e.detail.elt.querySelectorAll(".data-item")].forEach(option => {
                option.addEventListener("click", () => {
                    this.createSelectedItem(option);
                    option.style.display = "none";
                })
            })
            this.optionsWrapper.classList.add("active")
        });
    }
}

export const initSearch = (target) => {
    const container = target || document;
    [...container.querySelectorAll("[data-search]")].forEach(element => {
        if (element.dataset.search === "static") {
            new StaticSearch(element)
        } else {
            new DynamicSearch(element)
        }
    });
}
