(ert-deftest source-file-p-test ()
  (flet ((file-regular-p (a) t))
    (should (null (emamux-rt:source-file-p nil)))
    (should (null (emamux-rt:source-file-p "file.el")))
    (should (emamux-rt:source-file-p "file.rb"))))

(ert-deftest relative-file-name-test ()
  (flet ((emamux-rt:project-root () "/a/b/c/"))
    (should (equal "d" (emamux-rt:relative-file-name "/a/b/c/d")))))
