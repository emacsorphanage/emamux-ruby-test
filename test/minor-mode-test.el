(ert-deftest minor-mode-in-ruby-project-test ()
  (flet ((projectile-project-type () 'rails-test))
    (emamux-ruby-test-on)
    (should emamux-ruby-test-mode)))

(ert-deftest minor-mode-in-other-project-test ()
  (flet ((projectile-project-type () 'generic))
    (emamux-ruby-test-on)
    (should (null emamux-ruby-test-mode))))
