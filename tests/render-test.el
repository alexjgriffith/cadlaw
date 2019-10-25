(require 'el-mock)

(defvar tpr-psa-ess (with-temp-buffer
                      (insert-file-contents "TPR-PSA.xml")
                      (xml-parse-region (point-min) (point-max)))
  "Sample regulations to test on.")


(defconst sample-text-section-16 "Bar code
16(1) A primary package and a secondary package may display a bar code that is rectangular in shape, does not contain any image or design and is no more than 40 mm by 20 mm in size.
Bar code — manner of display
(2) The bar code may be displayed on a primary package and a secondary package if it is displayed in such a manner that the code
  (a) has a matte finish and is printed in either black and white or in drab brown and white;
  (b) is displayed only once on the primary package and once on the secondary package; and
  (c) in the case of a package that has a rectangular cuboid shape when it is closed, is displayed on only the exterior surface of the top, bottom or any one of the sides of the package.")

(ert-deftest render-section-t ()
  (should(string= sample-text-section-16
                  (cadlaw-apply-render (cadtrp-ess-get-section* "16") "" 't))))

(ert-deftest make-endpoint ()
  (should
   (string= (let ((cadlaw-endpoint-prefix "https://laws-lois.justice.gc.ca/eng/XML/")
                  (cadlaw-law-plist '(tpr-psa "SOR-2019-107.xml" excise-act "E-14.1.xml")))
              (cadlaw--make-endpoint 'excise-act))
            "https://laws-lois.justice.gc.ca/eng/XML/E-14.1.xml")))

(ert-deftest make-buffer-name ()
  (should
   (string= (let ((cadlaw-xml-buffer-prefix " *CADLAW XML Buffer - ")
                  (cadlaw-law-plist '(tpr-psa "SOR-2019-107.xml" excise-act "E-14.1.xml")))
              (cadlaw--make-buffer-name 'excise-act))
            " *CADLAW XML Buffer - E-14.1.xml")))

(ert-deftest get-keys ()
    (let ((sample-plist '(a 1 b 2 c 3)))
      (should (equalp '(a b c) (cadlaw--get-keys sample-plist)))))

