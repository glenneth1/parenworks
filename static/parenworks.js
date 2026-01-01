function toggleMobileNav() {
    var nav = document.querySelector('.nav-links');
    return nav ? nav.classList.toggle('active') : null;
};
function initSmoothScroll() {
    var links = document.querySelectorAll('a[href^=\'#\']');
    return links.forEach(function (link) {
        return link.addEventListener('click', function (e) {
            var href = link.getAttribute('href');
            var target = document.querySelector(href);
            if (target) {
                e.preventDefault();
                return target.scrollIntoView({ 'behavior' : 'smooth' });
            };
        });
    });
};
function validateContactForm(form) {
    var email = form.querySelector('input[type=\'email\']');
    var message = form.querySelector('textarea');
    return email.value.length && message.value.length;
};
function init() {
    initSmoothScroll();
    var contactForm = document.querySelector('.contact-form');
    if (contactForm) {
        __PS_MV_REG = [];
        return contactForm.addEventListener('submit', function (e) {
            if (!validateContactForm(contactForm)) {
                e.preventDefault();
                __PS_MV_REG = [];
                return console.log('Please fill in all required fields');
            };
        });
    };
};
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
} else {
    init();
};
