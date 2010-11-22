function checkForm(){
  if (!document.question.previousStimuliAnswerField[0].checked &&
      !document.question.previousStimuliAnswerField[1].checked) {
      // no radio button is selected
    alert('Please select an answer.');

    return false;
  }
  return true;
}

