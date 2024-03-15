const localStorageKey = "pool-assignment-messaging"

class AssignmentList {
    constructor(sessionId) {
        this.storeValuesInLocalstorage = this.storeValuesInLocalstorage.bind(this);
        this.readValuesFromLocalstorage = this.readValuesFromLocalstorage.bind(this);
        this.prependCheckboxes = this.prependCheckboxes.bind(this)
        this.toggleButtonClassnames = this.toggleButtonClassnames.bind(this)
        this.handleCancelClick = this.handleCancelClick.bind(this)
        this.afterSettleListener = this.afterSettleListener.bind(this)
        this.handleChangeCheckbox = this.handleChangeCheckbox.bind(this)
        this.handleSubmitClick = this.handleSubmitClick.bind(this)

        this.sessionId = sessionId;
        this.startSelectBtn = document.querySelector('[data-direct-message="select"]');
        this.cancelSelectBtn = document.querySelector('[data-direct-message="cancel"]');
        this.submitBtn = document.querySelector('[data-direct-message="submit"]');
        this.assignmentsTable = document.getElementById("assignments-table");
        const { isActive, values } = this.readValuesFromLocalstorage();

        this.isActive = isActive;
        this.checkboxesBuilt = false;
        this.values = values

        window.addEventListener("beforeunload", () => this.storeValuesInLocalstorage());
        this.readValuesFromLocalstorage();

        this.startSelectBtn.addEventListener("click", () => {
            this.isActive = true;
            this.prependCheckboxes();
            this.toggleButtonClassnames();
        })

        this.cancelSelectBtn.addEventListener("click", this.handleCancelClick)
        this.submitBtn.addEventListener('htmx:configRequest', this.handleSubmitClick)
        this.prependCheckboxes();
        this.toggleButtonClassnames();
        this.assignmentsTable.addEventListener("htmx:afterSettle", this.afterSettleListener)
    }

    getAssignmentRows() {
        return [...this.assignmentsTable.querySelectorAll("tr[data-id]")]
    }

    buildCheckbox(id) {
        const input = document.createElement("input");
        input.setAttribute("type", "checkbox");
        input.setAttribute("value", id)
        input.dataset.directMessageSelect = "";
        input.checked = Boolean(this.values[id])
        return input
    }

    handleChangeCheckbox(e) {
        const el = e.currentTarget;
        const id = el.value;
        if (el.checked) {
            this.values[id] = true;
        } else {
            let values = this.values
            delete values[id]
            this.values = this.values
        }
    }

    prependCheckboxes() {
        if (!this.isActive || this.checkboxesBuilt) {
            return
        }
        const rows = this.getAssignmentRows();
        rows.forEach(row => {
            const id = row.dataset.id;
            const cell = row.firstChild;
            const checkbox = this.buildCheckbox(id);
            checkbox.addEventListener("change", this.handleChangeCheckbox)
            cell.insertBefore(checkbox, cell.firstChild);
        })
        this.checkboxesBuilt = true;
    }


    storeValuesInLocalstorage() {
        const data = JSON.parse(localStorage.getItem(localStorageKey)) || {};
        data[this.sessionId] = {
            isActive: this.isActive,
            values: this.values
        }
        localStorage.setItem(localStorageKey, JSON.stringify(data))
    }

    afterSettleListener() {
        this.checkboxesBuilt = false;
        this.prependCheckboxes()
    }

    readValuesFromLocalstorage() {
        const stored = JSON.parse(localStorage.getItem(localStorageKey));
        const session = stored ? stored[this.sessionId] : null
        const values = (session && session.values) ? session.values : {}
        const isActive = session ? session.isActive : false;
        return { values, isActive }

    }

    toggleButtonClassnames() {
        if (this.isActive) {
            this.startSelectBtn.classList.add("hidden");
            this.cancelSelectBtn.classList.remove("hidden");
            this.submitBtn.classList.remove("hidden");
        } else {
            this.startSelectBtn.classList.remove("hidden");
            this.cancelSelectBtn.classList.add("hidden");
            this.submitBtn.classList.add("hidden");
        }
    }

    handleCancelClick() {
        [...this.assignmentsTable.querySelectorAll("[data-direct-message-select]")].forEach(el => el.remove());
        this.isActive = false;
        this.checkboxesBuilt = false;
        this.toggleButtonClassnames();
    }

    handleSubmitClick(e) {
        e.detail.parameters['assignment[]'] = Object.keys(this.values);
    }
}

export const initAssignmentListMessaging = (sessionId) => {
    new AssignmentList(sessionId)
}
