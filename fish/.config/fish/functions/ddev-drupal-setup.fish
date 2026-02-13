function ddev-drupal-setup
    ddev config --project-type=drupal11 --docroot=web && ddev start && ddev composer create-project drupal/cms && ddev launch
end
