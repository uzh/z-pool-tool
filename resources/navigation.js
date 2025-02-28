class OverlayNavigation {
    constructor(toggle, navigations) {
        this.toggleOpen = toggle.querySelector('[data-action="open"]');
        this.toggleClose = toggle.querySelector('[data-action="close"]');
        this.navigations = navigations;
        this._openNav = null;
        this.applyEvents();
    }

    set openNav(value) {
        const previous = this._openNav;
        this._openNav = value;
        this.onOpenNavChange(value, previous);
    }

    onOpenNavChange(current, previous) {
        const hideToggle = el => {
            el.classList.add('hidden');
            el.setAttribute('aria-hidden', 'true');
        }
        const showToggle = el => {
            el.classList.remove('hidden');
            el.setAttribute('aria-hidden', 'false');
        }
        if (previous) {
            this.closeNavigation(previous);
        }
        if (current) {
            document.body.setAttribute('data-noscroll', '');
            this.openNavigation(current);
            hideToggle(this.toggleOpen);
            showToggle(this.toggleClose);
        } else {
            document.body.removeAttribute('data-noscroll', '');
            showToggle(this.toggleOpen);
            hideToggle(this.toggleClose);
        }
    }

    openNavigation(nav) {
        nav.overlay.classList.add('active');
    }

    closeNavigation(nav) {
        nav.overlay.classList.remove('active');
    }

    applyEvents() {
        Object.values(this.navigations).forEach((nav) => {
            nav.toggle.addEventListener('click', () => {
                this.openNav = nav;
            });
        })
        this.toggleClose.addEventListener('click', () => this.openNav = null);
    }
}

export const initNavitation = () => {
    const headerToggle = document.getElementById('mobile-nav-toggle');
    if (!headerToggle) {
        console.error('Mobile navigation toggle not found');
        return;
    }
    const toggles = document.querySelectorAll('[data-overlay-nav]');
    const navigations = [...toggles].reduce((navs, toggle) => {
        const id = toggle.dataset.overlayNav;
        const overlay = document.getElementById(id);
        if (!overlay) {
            console.error('Navigation target not found');
            return navs;
        };
        return {
            ...navs, [id]: {
                overlay: overlay,
                toggle: toggle,
            }
        }
    }, {})
    new OverlayNavigation(headerToggle, navigations);
}
