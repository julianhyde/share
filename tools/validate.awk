/^\\/ {print}
/^\[(WARNING|ERROR)\] / {gsub(/^[^. ]* /, "");print}
