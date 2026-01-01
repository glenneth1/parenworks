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
function showStatus(message, isError) {
    var statusDiv = document.getElementById('form-status');
    if (statusDiv) {
        statusDiv.innerText = message;
        statusDiv.style.display = 'block';
        return statusDiv.className = isError ? 'form-status error' : 'form-status success';
    };
};
function submitContactForm(form) {
    var nameVal = form.querySelector('#name').value;
    var emailVal = form.querySelector('#email').value;
    var messageVal = form.querySelector('#message').value;
    var btn = document.getElementById('submit-btn');
    btn.disabled = true;
    btn.innerText = 'Sending...';
    __PS_MV_REG = [];
    return fetch('/api/parenworks/contact/submit', { 'method' : 'POST',
                                                     'headers' : { 'Content-Type' : 'application/x-www-form-urlencoded' },
                                                     'body' : 'name=' + encodeURIComponent(nameVal) + '&email=' + encodeURIComponent(emailVal) + '&message=' + encodeURIComponent(messageVal)
                                                   }).then(function (response) {
        return response.json();
    }).then(function (data) {
        if (data.error) {
            showStatus(data.message || 'Failed to send message', true);
            btn.disabled = null;
            __PS_MV_REG = [];
            return btn.innerHTML = '<span class=\"code-inline\">(send-message)</span>';
        } else {
            showStatus('Thank you! I\'ll get back to you soon.', null);
            form.reset();
            btn.disabled = null;
            __PS_MV_REG = [];
            return btn.innerHTML = '<span class=\"code-inline\">(send-message)</span>';
        };
    })['catch'](function (err) {
        console.log('Error:', err);
        showStatus('Failed to send message. Please try again.', true);
        btn.disabled = null;
        __PS_MV_REG = [];
        return btn.innerHTML = '<span class=\"code-inline\">(send-message)</span>';
    });
};
function init() {
    initSmoothScroll();
    var contactForm = document.getElementById('contact-form');
    __PS_MV_REG = [];
    return contactForm ? contactForm.addEventListener('submit', function (e) {
        e.preventDefault();
        __PS_MV_REG = [];
        return submitContactForm(contactForm);
    }) : null;
};
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
} else {
    init();
};
