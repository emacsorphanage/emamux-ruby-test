(ert-deftest tconsole-focused-correct-code-test ()
  (flet ((emamux-rt:focused-test () '(3 . "test_aaa_aaa"))
         (emamux-rt:focused-goal () '(1 . "Aaa::Bbb")))
    (should (equal "Aaa::Bbb#test_aaa_aaa" (emamux-rt:tconsole-focused-test)))))

(ert-deftest tconsole-focused-wrong-code-test ()
  (flet ((emamux-rt:focused-test () '(1 . "test_aaa_aaa"))
         (emamux-rt:focused-goal () '(3 . "Aaa::Bbb")))
    (should (null (emamux-rt:tconsole-focused-test)))))

(ert-deftest tconsole-focused-goal-test ()
  (flet ((emamux-rt:focused-goal () '(1 . "Aaa::Bbb")))
    (should (equal "Aaa::Bbb" (emamux-rt:tconsole-focused-goal)))))
