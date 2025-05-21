/*
 *  From Google AI / 2025-05-21
 **/

import { fetch } from 'std';

async function sendGetRequest(url) {
  try {
    const response = await fetch(url);

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const data = await response.text();
    return data;
  } catch (error) {
    console.error("Error fetching data:", error);
    return null;
  }
}

// Example usage:
const url = 'http://localhost:8080'; // Replace with your desired URL

sendGetRequest(url)
  .then(data => {
    if (data) {
      console.log("Response data:", data);
    } else {
      console.log("Failed to get data.");
    }
  });
